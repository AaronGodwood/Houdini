rtf_read_raw <- function(path) {
  lines <- tryCatch(
    readLines(path, encoding = "UTF-8", warn = FALSE),
    error = function(e)
      readLines(path, encoding = "latin1", warn = FALSE)
  )
  text <- paste(lines, collapse = "\n")
  #normalise any different line endings e.g. \r\n or \n
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  gsub("\r", "\n", text, fixed = TRUE)
}

# Resolve RTF escape sequences in a text string:
#   \'xx  -> the character for hex xx (in latin1, then to UTF-8)
#   \uN   -> Unicode code point N (followed by a fallback char we skip)
rtf_unescape_r <- function(text) {
  # Handle \uN escapes: find all, replace in reverse order to preserve positions
  m <- gregexpr("\\\\u(-?[0-9]+)\\??.", text, perl = TRUE)[[1]]
  if (m[1L] != -1L) {
    lens <- attr(m, "match.length")
    for (i in rev(seq_along(m))) {
      matched <- substr(text, m[i], m[i] + lens[i] - 1L)
      n <- suppressWarnings(as.integer(sub("\\\\u(-?[0-9]+).*", "\\1", matched)))
      if (is.na(n)) next
      if (n < 0L) n <- n + 65536L
      text <- paste0(substr(text, 1L, m[i] - 1L),
                     intToUtf8(n),
                     substr(text, m[i] + lens[i], nchar(text)))
    }
  }

  # Handle \'xx hex escapes the same way
  m <- gregexpr("\\\\'([0-9a-fA-F]{2})", text, perl = TRUE)[[1]]
  if (m[1L] != -1L) {
    lens <- attr(m, "match.length")
    for (i in rev(seq_along(m))) {
      hex <- substr(text, m[i] + 2L, m[i] + 3L)
      ch  <- rawToChar(as.raw(strtoi(hex, 16L)))
      ch  <- iconv(ch, from = "latin1", to = "UTF-8", sub = "?")
      text <- paste0(substr(text, 1L, m[i] - 1L),
                     ch,
                     substr(text, m[i] + lens[i], nchar(text)))
    }
  }

  text
}


#Finds the position of the closing brace "}" matching the opening brace "{" at 'start'
# Converts text to raw bytes for O(1) indexing instead of per character substr calls
find_matching_brace_r <- function(text, start){
  bytes <- charToRaw(text)
  n <- length(bytes)
  open <- charToRaw("{")
  close <- charToRaw("}")
  depth <- 0L
  pos <- start

  while( pos <= n) {
    b <- bytes[pos]
    if(b == open) depth <- depth + 1L
    else if (b == close){
      depth <- depth - 1L
      if (depth == 0L) return(pos)
    }
    pos <- pos + 1L
  }
  NA_integer_
}

# Remove all groups tagged with any given tags
remove_groups <- function(text, tags) {
  for (tag in tags){
    text <- remove_group(text, tag)
  }
  text
}

# -- Page Splitting

# Split RTF text into one element per page
# Uses \sectd to p[en each section and \sect to close it
rtf_split_pages <- function(text) {
  # Split on bare \sect
  parts <- strsplit(text, "\\\\sect(?![a-zA-Z])", perl = TRUE)[[1]]

  # Keep only chunks that contain \sectd (are real pages)
  parts <- parts[grepl("\\\\sectd", parts, perl = TRUE)]
}

# -- Group Extraction

# Find content of first group whose opening is tagged with 'tag'
# e.g. tag = "\\header" finds {\\header ...}
# Returns the inner text (without brackets) or NA if not found
extract_group <- function(text, tag){
  m <- regexpr(paste0("\\", tag, "(?![a-zA-Z])"), text, perl = TRUE)
  if(m == -1L) return( NA_character_)

  start <- m[1]
  brace_pos <- NA_integer_
  for( i in seq(start-1L, max(1L, start - 5L), by = -1L)){
    if(substr(text,i,i) == "{"){ brace_pos <- i; break}
  }
  if(is.na(brace_pos)) return(NA_character_)

  end_pos <- find_matching_brace_r(text, brace_pos)
  if(is.na(end_pos)) return(NA_character_)

  substr(text, brace_pos+1L, end_pos-1L)
}

# Remove all groups tagged with 'tag' from text
remove_group <- function(text, tag) {
  repeat {
    m <- regexpr(paste0("\\", tag, "(?![a-zA-Z])"), text, perl = TRUE)
    if (m == -1L) break

    start <- m[1]
    brace_pos <- NA_integer_
    for (i in seq(start - 1L, max(1L, start - 5L), by = -1L)) {
      if (substr(text, i, i) == "{") { brace_pos <- i; break }
    }
    if (is.na(brace_pos)) break

    end_pos <- find_matching_brace_r(text, brace_pos)
    if (is.na(end_pos)) break

    text <- paste0(substr(text, 1L, brace_pos - 1L),
                   substr(text, end_pos + 1L, nchar(text)))
  }
  text
}

# -- Table Parsing

# Given RTF text for one section (header or body), parse all rows.
# Returns a list of row objects:
#   list(is_header, cells = list(list(text, width_twips, colspan)))
parse_rtf_table <- function(section_text) {
  rows <- list()

  # Tokenise: find each \trowd ... \row block
  # We'll scan linearly through \trowd markers
  trowd_positions <- gregexpr("\\\\trowd(?![a-zA-Z])", section_text, perl = TRUE)[[1]]
  if (identical(trowd_positions, -1L)) return(rows)

  row_positions   <- gregexpr("\\\\row(?![a-zA-Z])",   section_text, perl = TRUE)[[1]]
  if (identical(row_positions, -1L))   return(rows)

  for (ti in seq_along(trowd_positions)) {
    row_start <- trowd_positions[ti]

    # Find the matching \row that comes after this \trowd
    row_end_candidates <- row_positions[row_positions > row_start]
    if (length(row_end_candidates) == 0L) next
    row_end <- row_end_candidates[1]

    row_text <- substr(section_text, row_start, row_end + 3L)  # include \row

    # --- is_header: \trhdr present? ---
    is_hdr <- grepl("\\\\trhdr(?![a-zA-Z])", row_text, perl = TRUE)

    # --- cell boundary positions (\cellxN) ---
    cellx_matches <- gregexpr("\\\\cellx([0-9]+)", row_text, perl = TRUE)
    cellx_vals <- as.integer(regmatches(row_text, cellx_matches)[[1]] |>
                               gsub("\\\\cellx", "", x = _))

    # --- merge flags per cell position ---
    # \clmgf = first of merge, \clmrg = continuation
    # These appear in the row definition before \cellxN values
    # We pair them by order with cellx_vals
    clmgf_pos <- gregexpr("\\\\clmgf(?![a-zA-Z])", row_text, perl = TRUE)[[1]]
    clmrg_pos <- gregexpr("\\\\clmrg(?![a-zA-Z])",  row_text, perl = TRUE)[[1]]
    has_clmgf <- !identical(clmgf_pos, -1L)
    has_clmrg <- !identical(clmrg_pos, -1L)

    # Build cell definition list (one entry per \cellx)
    n_cells_def <- length(cellx_vals)
    cell_defs <- vector("list", n_cells_def)
    for (ci in seq_len(n_cells_def)) {
      w <- if (ci == 1L) cellx_vals[1] else cellx_vals[ci] - cellx_vals[ci - 1L]
      cell_defs[[ci]] <- list(width_twips = w, is_merge_first = FALSE, is_merge_cont = FALSE)
    }

    # Tag merge cells: find \clmgf / \clmrg occurrences and their nearest following \cellx
    # Simpler approach: find all cell-def blocks between \trowd and first \cell
    # Each block starts at a \clmgf or \clmrg before its \cellxN
    if (has_clmgf || has_clmrg) {
      # Find positions of all \cellx in the row_text
      cx_pos <- gregexpr("\\\\cellx[0-9]+", row_text, perl = TRUE)[[1]]
      if (!identical(cx_pos, -1L)) {
        for (ci in seq_along(cx_pos)) {
          # What's between previous cellx (or \trowd) and this cellx?
          prev <- if (ci == 1L) 1L else cx_pos[ci - 1L]
          seg  <- substr(row_text, prev, cx_pos[ci])
          cell_defs[[ci]]$is_merge_first <- grepl("\\\\clmgf(?![a-zA-Z])", seg, perl = TRUE)
          cell_defs[[ci]]$is_merge_cont  <- grepl("\\\\clmrg(?![a-zA-Z])",  seg, perl = TRUE)
        }
      }
    }

    # --- extract cell contents (\cell boundaries) ---
    # Split on \cell to get cell content chunks
    cell_chunks <- strsplit(row_text, "\\\\cell(?![a-zA-Z])", perl = TRUE)[[1]]
    # Last chunk is after the final \cell (contains \row etc.), discard it
    if (length(cell_chunks) > 1L) cell_chunks <- cell_chunks[-length(cell_chunks)]

    n_cells_content <- length(cell_chunks)

    # --- bold fallback for header detection ---
    if (!is_hdr && n_cells_content > 0L) {
      non_empty <- cell_chunks[nchar(trimws(cell_chunks)) > 0L]
      if (length(non_empty) > 0L) {
        all_bold <- all(sapply(non_empty, function(ch) {
          # Has \b (bold on) and no \b0 (bold off) after it
          grepl("\\\\b(?![a-zA-Z0-9])", ch, perl = TRUE) &&
            !grepl("\\\\b0(?![a-zA-Z])", ch, perl = TRUE)
        }))
        if (all_bold) is_hdr <- TRUE
      }
    }

    # --- clean cell text ---
    cells <- vector("list", n_cells_content)
    for (ci in seq_len(n_cells_content)) {
      raw_cell <- cell_chunks[ci]

      # Extract alignment from RTF control words before stripping
      cell_align <- if (grepl("\\\\ql(?![a-zA-Z])", raw_cell, perl = TRUE)) {
        "left"
      } else if (grepl("\\\\qr(?![a-zA-Z])", raw_cell, perl = TRUE)) {
        "right"
      } else if (grepl("\\\\qc(?![a-zA-Z])", raw_cell, perl = TRUE)) {
        "center"
      } else {
        NA_character_
      }

      # Strip RTF control words and groups, leaving plain text
      cell_text <- rtf_cell_to_text_r(raw_cell)

      w_twips <- if (ci <= length(cell_defs)) cell_defs[[ci]]$width_twips else 0L
      is_cont  <- ci <= length(cell_defs) && isTRUE(cell_defs[[ci]]$is_merge_cont)
      is_first <- ci <= length(cell_defs) && isTRUE(cell_defs[[ci]]$is_merge_first)

      cells[[ci]] <- list(
        text           = cell_text,
        width_twips    = w_twips,
        align          = cell_align,
        is_merge_first = is_first,
        is_merge_cont  = is_cont
      )
    }

    # --- resolve merged cells: repeat first-cell text into continuations ---
    if (n_cells_content > 1L) {
      last_first_text <- ""
      for (ci in seq_len(n_cells_content)) {
        if (isTRUE(cells[[ci]]$is_merge_first)) {
          last_first_text <- cells[[ci]]$text
        } else if (isTRUE(cells[[ci]]$is_merge_cont)) {
          cells[[ci]]$text <- last_first_text
        }
      }
    }

    # --- compute colspan for each cell ---
    # A merged group: is_merge_first followed by N is_merge_cont cells -> colspan = N+1
    if (n_cells_content > 0L) {
      ci <- 1L
      while (ci <= n_cells_content) {
        if (isTRUE(cells[[ci]]$is_merge_first)) {
          span <- 1L
          j <- ci + 1L
          while (j <= n_cells_content && isTRUE(cells[[j]]$is_merge_cont)) {
            span <- span + 1L
            j <- j + 1L
          }
          cells[[ci]]$colspan <- span
          # Mark continuation cells with colspan 0 (to be skipped in output)
          for (k in seq(ci + 1L, length.out = span - 1L)) {
            if (k <= n_cells_content) cells[[k]]$colspan <- 0L
          }
          ci <- j
        } else {
          cells[[ci]]$colspan <- 1L
          ci <- ci + 1L
        }
      }
    }

    rows <- c(rows, list(list(is_header = is_hdr, cells = cells)))
  }

  rows
}

# Strip RTF markup from a cell's raw text, returning clean plain text
rtf_cell_to_text_r <- function(raw) {
  # Remove nested groups (e.g. field instructions, pictures)
  text <- raw

  # Remove {\*...} destination groups entirely
  text <- gsub("\\{\\\\\\*[^}]*\\}", "", text, perl = TRUE)

  # Iteratively strip innermost {...} groups: remove control words inside, keep plain text.
  # Two-pass per iteration: first strip control words inside the group, then remove braces.
  for (i in seq_len(20L)) {
    # Strip RTF control words inside innermost groups (no nested braces)
    new_text <- gsub("\\{((?:[^{}\\\\]|\\\\[a-zA-Z]+[-]?[0-9]*[ ]?)*)\\}",
                     "\\1", text, perl = TRUE)
    # Strip any remaining control words that were the only content
    new_text <- gsub("\\{\\\\[a-zA-Z]+[-]?[0-9]*[ ]?\\}", "", new_text, perl = TRUE)
    if (identical(new_text, text)) break
    text <- new_text
  }

  # Remove remaining RTF control words (\word or \word123)
  text <- gsub("\\\\[a-zA-Z]+[-]?[0-9]*\\s?", "", text, perl = TRUE)
  # Remove remaining control symbols (\<symbol>)
  text <- gsub("\\\\.", "", text, perl = TRUE)
  # Remove stray braces
  text <- gsub("[{}]", "", text, fixed = FALSE)

  # Apply unicode/hex unescaping on what remains
  text <- rtf_unescape_r(text)

  trimws(text)
}

# -- Page Parsing

parse_page <- function(page_text){
  # Extract header and footer groups, remaining is body
  header_text <- extract_group(page_text, "\\header")
  footer_text <- extract_group(page_text, "\\footer") # TODO make use of footer -- worked in old tokeniser

  body_text <- remove_groups(page_text, c(
    "\\header", "\\footer", "\\fonttbl", "\\colortbl")) # may need more -- test

  # Parse header table rows
  header_rows <- if (!is.na(header_text)) parse_rtf_table(header_text) else list()

  # Extract parameter for filtering
  # TODO
  parameter <- extract_parameter(header_rows)

  # Parse body table rows
  body_rows <- parse_rtf_table(body_text)

  # Split body rows into header rows (is_hdr == TRUE) and data rows
  # Note: header of page and the header of the table in the body are different
  is_hdr <- vapply(body_rows, `[[`, logical(1), "is_header")
  list(
    parameter = parameter,
    header_rows = body_rows[is_hdr],
    data_rows = body_rows[!is_hdr]  # TODO add in footnotes from footer
  )

}

# Extract "Parameter: <value>" from lowest row of RTF header section
extract_parameter <- function(header_rows) {
  if(length(header_rows) == 0L) return(NA_character_)

  # Check rows from bottom up
  for(i in rev(seq_along(header_rows))) {
    row <- header_rows[[i]]
    for(cell in row$cells) {
      m <- regmatches(cell$text,
                      regexpr("(?i)^Parameter:\\s*(.+)$", cell$text, perl = TRUE))
      if(length(m) > 0L && nchar(m) > 0L) {
        return(trimws(sub("(?i)^Parameter:\\s*", "", m, perl = TRUE)))
      }
    }
  }
  NA_character_
}


# -- top level parse function

#' Parse an RTF file into a list of page objects
#'
#' @param path Path to the .rtf file
#' @return List of page objects, each with:
#'   \item{parameter}{character or NA}
#'   \item{header_rows}{list of row objects}
#'   \item{data_rows}{list of row objects}
#'   Each row object: list(is_header, cells = list(list(text, width_twips, colspan, ...)))
parse_rtf <- function(path) {
  text  <- rtf_read_raw(path)
  pages <- rtf_split_pages(text)
  lapply(pages, parse_page)
}


#' Prepare a combined table with filtering and exclusions applied
#'
#' Common pipeline used by html.R and xml.R public functions.
#' @param pages Pre-parsed pages (from parse_rtf) or NULL
#' @param path RTF file path (used only if pages is NULL)
#' @param excluded_cols Integer vector of column indices to exclude
#' @param excluded_rows Integer vector of data row indices to exclude
#' @param excluded_header_rows Integer vector of header row indices to exclude
#' @param parameters Parameter filter
#' @param timelines Timeline filter
#' @return list(combined, included_cols) where combined has rows filtered
prepare_table <- function(pages = NULL, path = NULL,
                          excluded_cols = NULL, excluded_rows = NULL,
                          excluded_header_rows = NULL,
                          parameters = NULL, timelines = NULL) {
  if (is.null(pages)) pages <- parse_rtf(path)
  pages    <- filter_pages(pages, parameters)
  #TODO timeline filtering
  combined <- combine_pages(pages)

  n_cols <- length(combined$col_widths_twips)
  n_data <- length(combined$data_rows)
  n_hdr  <- length(combined$header_rows)

  inc_cols <- setdiff(seq_len(n_cols), excluded_cols        %||% integer())  # can be used for Mark's request
  inc_rows <- setdiff(seq_len(n_data), excluded_rows        %||% integer())
  inc_hdrs <- setdiff(seq_len(n_hdr),  excluded_header_rows %||% integer())

  combined$data_rows   <- combined$data_rows[inc_rows]
  combined$header_rows <- combined$header_rows[inc_hdrs]

  list(combined = combined, included_cols = inc_cols)
}


# --filtering

#' Filter pages by parameter value
#'
#' Pages with NA parameter are always kept.
#' @param pages Output of parse_rtf()
#' @param parameters Character vector of parameter values to include
filter_pages <- function(pages, parameters) {
  if (is.null(parameters) || length(parameters) == 0L) return(pages)
  pages[vapply(pages, function(p) {
    is.na(p$parameter) || p$parameter %in% parameters
  }, logical(1))]
}




#' Get all unique parameter values across pages
#'
#' @param pages Output of parse_rtf()
#' @return Character vector of unique parameter values
get_parameters <- function(pages) {
  params <- vapply(pages, `[[`, character(1), "parameter")
  unique(params[!is.na(params)])
}


# --combining pages

#' Combine multiple pages into a single flat table
#'
#' Header rows are taken from the first page only.
#' Data rows are concatenated across all pages.
#' @param pages Filtered list of page objects
#' @return list(header_rows, data_rows, col_widths_twips)
combine_pages <- function(pages) {
  if (length(pages) == 0L) {
    return(list(header_rows = list(), data_rows = list(), col_widths_twips = numeric()))
  }

  header_rows <- pages[[1]]$header_rows
  data_rows   <- unlist(lapply(pages, `[[`, "data_rows"), recursive = FALSE)

  # Column widths from first page's first data (or header) row
  ref_rows <- if (length(header_rows) > 0L) header_rows else data_rows
  col_widths_twips <- if (length(ref_rows) > 0L) {
    vapply(ref_rows[[1]]$cells, `[[`, numeric(1), "width_twips")
  } else {
    numeric()
  }

  list(
    header_rows      = header_rows,
    data_rows        = data_rows,
    col_widths_twips = col_widths_twips
  )
}

