
# Whether the compiled C fast-path is available. Resolved lazily on first use
# and cached: the DLL registered by useDynLib() is not guaranteed to be loaded
# at the moment this file is sourced during namespace construction, so an eager
# check here can spuriously report FALSE and silently disable the C path.
.c_available <- local({
  cached <- NA
  function() {
    if (!is.na(cached)) return(cached)
    cached <<- tryCatch({
      getNativeSymbolInfo("C_find_matching_brace", PACKAGE = "Houdini")
      TRUE
    }, error = function(e) FALSE)
    cached
  }
})




# Read raw bytes and convert to UTF-8 string, handling CP1252/Latin-1.
# readLines(encoding=) never errors on wrong encodings, it just mis-marks the
# string - so detect real UTF-8 with validUTF8() and convert otherwise.
rtf_read_raw <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.size(path))
  bytes <- bytes[bytes != as.raw(0L)]
  text  <- rawToChar(bytes)
  if (validUTF8(text)) {
    Encoding(text) <- "UTF-8"
  } else {
    # SAS/Word on Windows write CP1252 (latin1 plus smart quotes, dashes, ...)
    text <- iconv(text, from = "CP1252", to = "UTF-8", sub = "?")
  }
  # Normalise any residual \r\n or \r
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  gsub("\r",   "\n", text, fixed = TRUE)
}

# Resolve RTF escape sequences in a text string:
#   \'xx  -> the character for hex xx (CP1252, converted to UTF-8)
#   \uN   -> Unicode code point N, followed by \ucN fallback chars we skip
#   \ucN  -> sets the fallback length for subsequent \uN escapes (default 1);
#            consumed and not emitted
# A fallback "character" may itself be a \'xx escape (counts as one).
rtf_unescape_r <- function(text) {
  if (!grepl("\\\\(u|')", text, perl = TRUE)) return(text)

  n   <- nchar(text)
  out <- character()
  pos <- 1L
  uc  <- 1L

  repeat {
    m <- regexpr("\\\\(uc[0-9]+|u-?[0-9]+|'[0-9a-fA-F]{2})",
                 substr(text, pos, n), perl = TRUE)
    if (m == -1L) {
      out <- c(out, substr(text, pos, n))
      break
    }
    start <- pos + m[1L] - 1L
    len   <- attr(m, "match.length")
    out   <- c(out, substr(text, pos, start - 1L))
    tok   <- substr(text, start, start + len - 1L)
    pos   <- start + len

    if (startsWith(tok, "\\uc")) {
      uc <- as.integer(substr(tok, 4L, len))
      if (substr(text, pos, pos) == " ") pos <- pos + 1L  # delimiter
    } else if (startsWith(tok, "\\u")) {
      cp <- as.integer(substr(tok, 3L, len))
      if (cp < 0L) cp <- cp + 65536L
      out <- c(out, intToUtf8(cp))
      if (substr(text, pos, pos) == " ") pos <- pos + 1L  # delimiter
      # Skip the uc fallback characters; stop early at structure we shouldn't eat
      k <- uc
      while (k > 0L && pos <= n) {
        ch <- substr(text, pos, pos)
        if (ch == "\\") {
          if (substr(text, pos + 1L, pos + 1L) == "'") pos <- pos + 4L else break
        } else if (ch == "{" || ch == "}") {
          break
        } else {
          pos <- pos + 1L
        }
        k <- k - 1L
      }
    } else {
      # \'xx hex escape
      hex <- substr(tok, 3L, 4L)
      ch  <- rawToChar(as.raw(strtoi(hex, 16L)))
      out <- c(out, iconv(ch, from = "CP1252", to = "UTF-8", sub = "?"))
    }
  }

  paste(out, collapse = "")
}

rtf_unescape <- function(text) {
  if (.c_available()) .Call(C_rtf_unescape, text) else rtf_unescape_r(text)
}

# Fast hex string to raw vector conversion.
# Processes in chunks of 4000 bytes (8000 hex chars) to limit intermediate
# string allocation compared to one-pair-at-a-time substring calls.
hex_to_raw <- function(hex) {
  n <- nchar(hex) %/% 2L
  chunk <- 4000L
  parts <- vector("list", ceiling(n / chunk))
  pi <- 1L
  pos <- 1L
  while (pos <= n * 2L) {
    end <- min(pos + chunk * 2L - 1L, n * 2L)
    piece <- substr(hex, pos, end)
    nb <- nchar(piece) %/% 2L
    starts <- seq(1L, by = 2L, length.out = nb)
    parts[[pi]] <- as.raw(strtoi(substring(piece, starts, starts + 1L), 16L))
    pi <- pi + 1L
    pos <- end + 1L
  }
  unlist(parts)
}


# Find the position of the closing brace matching the opening brace at `start`.
# Converts to raw bytes for O(1) indexing instead of per-character substr calls.
find_matching_brace_r <- function(text, start) {
  bytes <- charToRaw(text)
  n <- length(bytes)
  open  <- charToRaw("{")
  close <- charToRaw("}")
  depth <- 0L
  pos <- start
  while (pos <= n) {
    b <- bytes[pos]
    if (b == open) depth <- depth + 1L
    else if (b == close) {
      depth <- depth - 1L
      if (depth == 0L) return(pos)
    }
    pos <- pos + 1L
  }
  NA_integer_
}

find_matching_brace <- function(text, start) {
  if (.c_available()) .Call(C_find_matching_brace, text, as.integer(start))
  else find_matching_brace_r(text, start)
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

  end_pos <- find_matching_brace(text, brace_pos)
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

    end_pos <- find_matching_brace(text, brace_pos)
    if (is.na(end_pos)) break

    text <- paste0(substr(text, 1L, brace_pos - 1L),
                   substr(text, end_pos, nchar(text)))
  }
  text
}

# -- Table Blocks

#
# A "block" is a rectangular set of table rows stored column-wise as matrices
# (one row per table row), rather than nested lists of per-cell objects:
#   text    - character (n x k), "" where a row has no cell in that column
#   align   - character (n x k), NA where the cell declares no alignment
#   colspan - integer   (n x k), 1 normal, >1 merge head, 0 merge continuation
#   width   - integer   (n x k), cell width in twips
#   present - logical   (n x k), FALSE where row r has no cell in column c
#   is_header - logical(n)
#   row_id    - integer(n), stable identity assigned by parse_rtf (NA before)
# Filtering and combining reduce to matrix subsetting, and a large document
# is a handful of big vectors instead of millions of tiny list objects.

block_new <- function() {
  m_chr <- matrix(character(), 0L, 0L)
  m_int <- matrix(integer(),   0L, 0L)
  list(
    text = m_chr, align = m_chr, colspan = m_int, width = m_int,
    present = matrix(logical(), 0L, 0L),
    is_header = logical(), row_id = integer()
  )
}

block_nrow <- function(b) length(b$is_header)
block_ncol <- function(b) ncol(b$text)


block_rows_id <- function(b, idx){
  b$text      <- b$text[b$row_id %in% idx, , drop = FALSE]
  b$align     <- b$align[b$row_id %in% idx, , drop = FALSE]
  b$colspan   <- b$colspan[b$row_id %in% idx, , drop = FALSE]
  b$width     <- b$width[b$row_id %in% idx, , drop = FALSE]
  b$present   <- b$present[b$row_id %in% idx, , drop = FALSE]
  b$is_header <- b$is_header[b$row_id %in% idx]
  b$row_id    <- b$row_id[b$row_id %in% idx]
  b
}


# Subset a block to the given rows (logical or integer index)
block_rows <- function(b, idx) {
  b$text      <- b$text[idx, , drop = FALSE]
  b$align     <- b$align[idx, , drop = FALSE]
  b$colspan   <- b$colspan[idx, , drop = FALSE]
  b$width     <- b$width[idx, , drop = FALSE]
  b$present   <- b$present[idx, , drop = FALSE]
  b$is_header <- b$is_header[idx]
  b$row_id    <- b$row_id[idx]
  b
}

# Widen a block to at least k columns, filling with "absent cell" values
block_pad <- function(b, k) {
  extra <- k - block_ncol(b)
  if (extra <= 0L) return(b)
  n <- block_nrow(b)
  b$text    <- cbind(b$text,    matrix("",            n, extra))
  b$align   <- cbind(b$align,   matrix(NA_character_, n, extra))
  b$colspan <- cbind(b$colspan, matrix(0L,            n, extra))
  b$width   <- cbind(b$width,   matrix(0L,            n, extra))
  b$present <- cbind(b$present, matrix(FALSE,         n, extra))
  b
}


# Subset a block to the given column indices (row fields unchanged)
block_cols <- function(b, cols) {
  if (block_nrow(b) == 0L) return(b)
  b <- block_pad(b, max(cols, 0L))
  b$text    <- b$text[, cols, drop = FALSE]
  b$align   <- b$align[, cols, drop = FALSE]
  b$colspan <- b$colspan[, cols, drop = FALSE]
  b$width   <- b$width[, cols, drop = FALSE]
  b$present <- b$present[, cols, drop = FALSE]
  b
}


#shifts spanning data across appropriat;y when columns are removed then removes them
block_cols_resolve <- function(b, cols) {
  if (block_nrow(b) == 0L) return(b)
  b <- block_pad(b, max(cols, 0L))
  n_cols <- block_ncol(b)
  n_rows <- block_nrow(b)
  condemned_cols <- setdiff(seq_len(n_cols),cols)
  mask <- logical(n_cols)
  mask[condemned_cols] <- TRUE
  for(i in seq_len(n_rows)){
    j <- 1
    up <- TRUE
    while(j<= n_cols){
      #only act if column is in mask, value > 1 and not last col
      if(mask[j] && b$colspan[i,j] > 1 && j < n_cols){
        b$text[i, j + 1]    <- b$text[i, j]
        b$align[i, j + 1]   <- b$align[i, j]
        b$colspan[i, j + 1] <- b$colspan[i, j] - 1
        b$width[i, j + 1]   <- b$width[i, j]
        b$present[i, j + 1] <- b$present[i, j]
        j <- j + 1
      } else if(mask[j] && b$colspan[i,j] == 0 && j > 1 && up == TRUE){
        up <- FALSE
        k <- j - 1
      } else if( up == FALSE){

        if(b$colspan[i,k] > 0){
          b$colspan[i,k] <- b$colspan[i,k] - 1
          up = TRUE
          j <- j + 1
        } else{
          k <- k - 1
        }
      } else{
        j <- j + 1
      }


    }
  }
  block_cols(b, cols)
}


# Stack blocks vertically, padding narrower blocks with absent cells
block_rbind_all <- function(blocks) {
  blocks <- blocks[vapply(blocks, block_nrow, integer(1)) > 0L]
  if (length(blocks) == 0L) return(block_new())
  if (length(blocks) == 1L) return(blocks[[1L]])
  k <- max(vapply(blocks, block_ncol, integer(1)))
  blocks <- lapply(blocks, block_pad, k = k)
  list(
    text      = do.call(rbind, lapply(blocks, `[[`, "text")),
    align     = do.call(rbind, lapply(blocks, `[[`, "align")),
    colspan   = do.call(rbind, lapply(blocks, `[[`, "colspan")),
    width     = do.call(rbind, lapply(blocks, `[[`, "width")),
    present   = do.call(rbind, lapply(blocks, `[[`, "present")),
    is_header = unlist(lapply(blocks, `[[`, "is_header"), use.names = FALSE),
    row_id    = unlist(lapply(blocks, `[[`, "row_id"),    use.names = FALSE)
  )
}

# Tables get double gaps when pages get combined this removes one of those spaces
# Some tables (particularly AEs) have (cont.) sections where a chunk is CONTINUED over a page
# MW are not a fan of this so this removed them and any gaps caused by this same page break
#
# TODO consult someone about this May cause random errors but I do feel it is unlikely
block_categorise <- function(block){
  n_col <- block_ncol(block)
  if(n_col == 0) return(block)
  spans <- block$colspan[ ,1]
  empty <- which(vapply(spans, function(s) s == n_col, logical(1)))
  #cont <- which(vapply(block$text[ ,1], function(t) grepl("(cont.)",t), logical(1)))
  two_empty <- empty[(empty + 1) %in% empty]
  #cont_empty <- empty[((empty + 1) %in% cont | (empty + 2) %in% cont)]
  ids <- block$row_id
  wanted_ids <- ids[!ids %in% two_empty]# & !ids %in% cont_empty & !ids %in% cont]
  block_rows_id(block, wanted_ids)
}

# -- Table Parsing


# findInterval re-validates `vec` on every call (anyNA + is.unsorted + an
# as.double copy) - O(length(vec)) work that turns per-row lookups over large
# sections quadratic. Positions here are always sorted doubles, so skip the
# checks where this R version allows it (R >= 4.3).
fint <- if (getRversion() >= "4.3.0") {
  function(x, vec) findInterval(x, vec)#, checkSorted = FALSE, checkNA = FALSE)
} else {
  findInterval
}

# Start positions of all matches of `pattern`, ascending; numeric(0) if none.
# Doubles, not integers, so fint() avoids a per-call as.double copy.
token_positions <- function(text, pattern) {
  p <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (p[1L] == -1L) numeric() else as.double(p)
}


# Elements of the sorted position vector `pos` that fall within [lo, hi]
pos_within <- function(pos, lo, hi) {
  if (length(pos) == 0L) return(numeric())
  i1 <- fint(lo - 1L, pos) + 1L
  i2 <- fint(hi, pos)
  if (i1 > i2) numeric() else pos[i1:i2]
}

# Does any element of the sorted position vector `pos` fall within [lo, hi]?
any_within <- function(pos, lo, hi) {
  length(pos) != 0L && fint(hi, pos) > fint(lo - 1L, pos)
}

grid_align_row <- function(texts, aligns, flag_first, flag_cont, ge, grid_widths){

  n_c <- length(texts)
  n_def <- length(ge)
  size <- length(grid_widths) + n_c
  text <- character(size)
  align <- rep(NA_character_, size)
  span <- integer(size)
  width <- integer(size)
  present <- integer(size)

  used <- 0L #rightmost output col written
  prev_end <- 0L #rightmost grid col covered so far
  ci <- 1L
  while(ci <= n_c){
    if(ci <= n_def){
      j <- ci +1L
      if(flag_first[ci]) while(j <= n_c && flag_cont[j]) j <- j + 1L
      end <- ge[min(j-1L, n_def)]
      s <- end- prev_end
      if (s > 0L){
        gs <- prev_end + 1L
        idx <- gs:end
        present[idx] <- TRUE
        text[idx] <- texts[ci]
        width[idx] <- grid_widths[idx]
        align[gs] <- aligns[ci]
        span[gs] <- s
        prev_end <- end
        used <- end
      }
      ci <- j
    } else {

      used <- used + 1L
      present[used] <- TRUE
      text[used] <- texts[ci]
      align[used] <- aligns[ci]
      span[used] <- 1L
      ci <- ci + 1L
    }
  }

  idx <- seq_len(used)
  list(text = text[idx], align = align[idx], span = span[idx],
       width = width[idx], present = present[idx])

}

# Given RTF text for one section (header or body), parse all rows.
# Returns a block (see section above).
#
# Each token type is located with a single regex scan over the whole section;
# per-row and per-cell tests then become sorted-position lookups instead of
# fresh regex passes over row substrings.
parse_rtf_table <- function(section_text, hide_data = FALSE) {
  trowd_pos <- token_positions(section_text, "\\\\trowd(?![a-zA-Z])")
  if (length(trowd_pos) == 0L) return(block_new())

  row_pos <- token_positions(section_text, "\\\\row(?![a-zA-Z])")
  if (length(row_pos) == 0L) return(block_new())

  trhdr_pos    <- token_positions(section_text, "\\\\trhdr(?![a-zA-Z])")
  clmgf_pos    <- token_positions(section_text, "\\\\clmgf(?![a-zA-Z])")
  clmrg_pos    <- token_positions(section_text, "\\\\clmrg(?![a-zA-Z])")
  cell_pos     <- token_positions(section_text, "\\\\cell(?![a-zA-Z])")
  ql_pos       <- token_positions(section_text, "\\\\ql(?![a-zA-Z])")
  qr_pos       <- token_positions(section_text, "\\\\qr(?![a-zA-Z])")
  qc_pos       <- token_positions(section_text, "\\\\qc(?![a-zA-Z])")
  bold_on_pos  <- token_positions(section_text, "\\\\b(?![a-zA-Z0-9])")
  bold_off_pos <- token_positions(section_text, "\\\\b0(?![a-zA-Z])")

  cellx_m   <- gregexpr("\\\\cellx([0-9]+)", section_text, perl = TRUE)
  cellx_pos <- cellx_m[[1]]
  if (cellx_pos[1L] == -1L) {
    cellx_pos  <- numeric()
    cellx_vals <- integer()
  } else {
    cellx_pos  <- as.double(cellx_pos)
    cellx_vals <- as.integer(gsub("\\\\cellx", "",
                                  regmatches(section_text, cellx_m)[[1]]))
  }

  n_max      <- length(trowd_pos)
  row_texts  <- vector("list", n_max)
  row_aligns <- vector("list", n_max)
  row_ff  <- vector("list", n_max) #merge first flag
  row_fc <- vector("list", n_max) #merge continuation flag
  row_bounds <- vector("list", n_max) #cumulative cell bounds
  row_hdr    <- logical(n_max)
  nr         <- 0L

  for (ti in seq_along(trowd_pos)) {
    row_start <- trowd_pos[ti]

    # First \row after this \trowd closes the row
    ri <- fint(row_start, row_pos) + 1L
    if (ri > length(row_pos)) next
    row_close <- row_pos[ri] + 3L  # last char of "\row"

    # --- is_header: \trhdr present? ---
    is_hdr <- any_within(trhdr_pos, row_start, row_close)

    # --- cell boundary positions (\cellxN) ---
    di1 <- fint(row_start - 1L, cellx_pos) + 1L
    di2 <- fint(row_close, cellx_pos)
    if (di1 <= di2) {
      cx_pos_row <- cellx_pos[di1:di2]
      cx_vals    <- cellx_vals[di1:di2]
    } else {
      cx_pos_row <- numeric()
      cx_vals    <- integer()
    }

    n_cells_def <- length(cx_vals)


    # --- merge flags per cell position ---
    # \clmgf = first of merge, \clmrg = continuation; each belongs to the
    # next \cellx that follows it in the row definition
    merge_first <- logical(n_cells_def)
    merge_cont  <- logical(n_cells_def)
    for (p in pos_within(clmgf_pos, row_start, row_close)) {
      k <- fint(p, cx_pos_row) + 1L
      if (k <= n_cells_def) merge_first[k] <- TRUE
    }
    for (p in pos_within(clmrg_pos, row_start, row_close)) {
      k <- fint(p, cx_pos_row) + 1L
      if (k <= n_cells_def) merge_cont[k] <- TRUE
    }

    # --- extract cell contents (\cell boundaries) ---
    # Chunk i runs from just after the previous \cell (or the row start)
    # up to just before \cell i; text after the final \cell is discarded.
    # A row with no \cell yields one chunk spanning the whole row.
    cp <- pos_within(cell_pos, row_start, row_close)
    if (length(cp) > 0L) {
      chunk_starts <- c(row_start, cp[-length(cp)] + 5L)
      chunk_ends   <- cp - 1L
    } else {
      chunk_starts <- row_start
      chunk_ends   <- row_close
    }
    cell_chunks <- substring(section_text, chunk_starts, chunk_ends)

    n_cells_content <- length(cell_chunks)

    # Merge flags per content cell (FALSE beyond the defined cells)
    flag_first <- logical(n_cells_content)
    flag_cont  <- logical(n_cells_content)
    nd <- min(n_cells_content, n_cells_def)
    if (nd > 0L) {
      flag_first[seq_len(nd)] <- merge_first[seq_len(nd)]
      flag_cont[seq_len(nd)]  <- merge_cont[seq_len(nd)]
    }

    # --- bold fallback for header detection ---
    if (!is_hdr && n_cells_content > 0L) {
      # has any non-whitespace character (= nchar(trimws(x)) > 0, but one
      # vectorised regex call instead of trimws's two sub() calls per row)
      non_empty <- which(grepl("[^ \t\r\n]", cell_chunks, perl = TRUE))
      if (length(non_empty) > 0L) {
        all_bold <- TRUE
        for (ci in non_empty) {
          # Has \b (bold on) and no \b0 (bold off)
          if (!any_within(bold_on_pos, chunk_starts[ci], chunk_ends[ci]) ||
              any_within(bold_off_pos, chunk_starts[ci], chunk_ends[ci])) {
            all_bold <- FALSE
            break
          }
        }
        if (all_bold) is_hdr <- TRUE
      }
    }

    # --- clean cell text ---
    texts <- vapply(cell_chunks, function(c) rtf_cell_to_text(c, hide_data), character(1),
                    USE.NAMES = FALSE)

    # Alignment from RTF control words within each cell chunk
    aligns <- rep(NA_character_, n_cells_content)
    for (ci in seq_len(n_cells_content)) {
      cs <- chunk_starts[ci]
      ce <- chunk_ends[ci]
      aligns[ci] <- if (any_within(ql_pos, cs, ce)) {
        "left"
      } else if (any_within(qr_pos, cs, ce)) {
        "right"
      } else if (any_within(qc_pos, cs, ce)) {
        "center"
      } else {
        NA_character_
      }
    }


    nr <- nr + 1L
    row_texts[[nr]]  <- texts
    row_aligns[[nr]] <- aligns
    row_ff[[nr]]  <- flag_first
    row_fc[[nr]]  <- flag_cont
    row_bounds[[nr]]  <- cx_vals
    row_hdr[nr]      <- is_hdr
  }


  # --- column grid

  all_b <- sort(unique(as.double(unlist(row_bounds[seq_len(nr)]))))
  if(length(all_b) > 0L){
    cl <- cumsum(c(1L, as.integer(diff(all_b) > 10)))
    is_last <- c(cl[-1L] != cl[-length(cl)], TRUE)
    grid_widths <- as.integer(round(diff(c(0, all_b[is_last]))))
  } else {
    cl <- integer()
    grid_widths <- integer()
  }


  # -- express every row on the grid
  rows <- vector("list", nr)
  for(i in seq_len(nr)){
    bounds <- row_bounds[[i]]
    ge <- if (length(bounds) > 0L) cl[fint(bounds, all_b)] else integer()
    rows[[i]] <- grid_align_row(row_texts[[i]], row_aligns[[i]],row_ff[[i]],
                                row_fc[[i]], ge, grid_widths)
  }

  # --- assemble the block matrices ---
  lens <- vapply(rows, function(r) length(r$text), integer(1))
  k <- if (nr > 0L) max(lens, 0L) else 0L
  text    <- matrix("",            nr, k)
  align   <- matrix(NA_character_, nr, k)
  colspan <- matrix(0L,            nr, k)
  width   <- matrix(0L,            nr, k)
  present <- matrix(FALSE,         nr, k)
  for (i in seq_len(nr)) {
    if (lens[i] > 0L) {
      idx <- seq_len(lens[i])
      text[i, idx]    <- rows[[i]]$text
      align[i, idx]   <- rows[[i]]$align
      colspan[i, idx] <- rows[[i]]$span
      width[i, idx]   <- rows[[i]]$width
      present[i, idx] <- rows[[i]]$present
    }
  }

  list(
    text = text, align = align, colspan = colspan, width = width,
    present = present,
    is_header = row_hdr[seq_len(nr)],
    row_id    = rep(NA_integer_, nr)
  )
}


# Conversions from \\li in RTF so that correct indenting in maintained
# Important for level filtering
TWIPS_PER_INDENT_LEVEL <- 194L

SPACES_PER_INDENT_LEVEL <- 2L

#Catches \\li indents and converts them to a number of spaces via conversions above
rtf_indent_prefix <- function(text){
  m <- regmatches(text, gregexpr("\\\\li(-?[0-9]+)(?![0-9])", text, perl = TRUE))[[1L]]
  if(length(m) == 0L) return("")
  twips <- as.integer(sub("\\\\li","",m))
  twips <- twips[!is.na(twips) & twips > 0L]
  if(length(twips) == 0L) return("")
  level <- round(min(twips) / TWIPS_PER_INDENT_LEVEL)
  strrep(" ", level * SPACES_PER_INDENT_LEVEL)
}

# Strip RTF markup from a cell's raw text, returning clean plain text
rtf_cell_to_text_r <- function(raw, hide_data = FALSE) {
  # Remove nested groups (e.g. field instructions, pictures)
  text <- raw

  # Remove {\*...} destination groups entirely
  text <- gsub("\\{\\\\\\*[^}]*\\}", "", text, perl = TRUE)
  indent <- rtf_indent_prefix(text)


  # Iteratively strip innermost {...} groups: remove control words inside, keep plain text.
  # Two-pass per iteration: first strip control words inside the group, then remove braces.
  # for (i in seq_len(20L)) {
  #   # Strip RTF control words inside innermost groups (no nested braces)
  #   new_text <- gsub("\\{((?:[^{}\\\\]|\\\\[a-zA-Z]+[-]?[0-9]*[ ]?)*)\\}",
  #                    "\\1", text, perl = TRUE)
  #   # Strip any remaining control words that were the only content
  #   # (but not \uN / \ucN, which rtf_unescape decodes later)
  #   new_text <- gsub("\\{\\\\(?!u-?[0-9]|uc[0-9])[a-zA-Z]+[-]?[0-9]*[ ]?\\}", "", new_text, perl = TRUE)
  #   if (identical(new_text, text)) break
  #   text <- new_text
  # }

  #remove rtf generated \n s and then replace rtf \\line control words with \n
  text <- gsub("[\r\n]", "", text, perl = TRUE)
  text <- gsub("\\\\line(?![a-zA-A])\\s?", "\n", text, perl = TRUE)

  # Remove remaining RTF control words (\word or \word123), but preserve
  # \uN and \ucN - they are decoded (not stripped) by rtf_unescape below
  text <- gsub("\\\\(?!u-?[0-9]|uc[0-9])[a-zA-Z]+[-]?[0-9]*[ ]?", "", text, perl = TRUE)
  # Replace \\~ with spaces (appears in some tables as a space escape character)
  text <- gsub("\\\\~"," ", text, perl = TRUE)
  # Remove remaining control symbols (\<symbol>), preserving \'xx hex escapes
  text <- gsub("\\\\(?!')[^a-zA-Z]", "", text, perl = TRUE)
  # Remove stray brace,s
  text <- gsub("[{}]", "", text, fixed = FALSE)

  # Apply unicode/hex unescaping on what remains
  text <- paste0(indent, rtf_unescape(text))
  if(hide_data && grepl("^[0-9]", text)){
    return("XX")
  }
  text
}

rtf_cell_to_text <- function(raw, hide_data = FALSE) {
  if (.c_available()) .Call(C_rtf_cell_to_text, raw, hide_data) else rtf_cell_to_text_r(raw, hide_data)
}

# -- Page Parsing

parse_page <- function(page_text, hide_data = FALSE){
  # Extract header and footer groups, remaining is body
  header_text <- extract_group(page_text, "\\header")
  footer_text <- extract_group(page_text, "\\footer")

  body_text <- remove_groups(page_text, c(
    "\\header", "\\footer", "\\fonttbl", "\\colortbl","\\stylesheet","\\info"))

  # Parse the page-header table and pull the parameter from its lowest row
  header_tbl <- if (!is.na(header_text)) {
    parse_rtf_table(header_text)
  } else {
    block_new()
  }
  parameter <- extract_parameter(header_tbl)

  footer_tbl <- if (!is.na(footer_text)) {
    extract_footnotes(parse_rtf_table(footer_text))
  } else {
    block_new()
  }



  # Parse the body table and split into header rows and data rows
  body <- parse_rtf_table(body_text, hide_data = hide_data)
  footer_tbl$colspan <- matrix(block_ncol(body))
  list(
    parameter = parameter,
    header    = block_rows(body, body$is_header),
    data      = block_rows(body, !body$is_header),
    footer    = footer_tbl
  )

}

# Extract "Parameter: <value>" from the lowest row of the RTF header section
extract_parameter <- function(header_tbl) {
  for (i in rev(seq_len(block_nrow(header_tbl)))) {
    for (txt in header_tbl$text[i, header_tbl$present[i, ]]) {
      m <- regmatches(txt, regexpr("(?i)^[^:]+:\\s*(.+)$", txt, perl = TRUE))
      if (length(m) > 0L && nchar(m) > 0L) {
        return(trimws(sub("(?i)^[^:]+:\\s*", "", m, perl = TRUE)))
      }
    }
  }
  NA_character_
}

# Extract Footnotes from the RTF footer section (everything above first blank row)
extract_footnotes <- function(footer_tbl){
  first_blank <- which(vapply(footer_tbl$text, function(t) t =="", logical(1)))[1]
  if(is.na(first_blank) || first_blank > 2) return(block_new())
  block_rows(footer_tbl,seq_len(first_blank-1))
}




# -- top level parse function

#' Parse an RTF file into a list of page objects
#'
#' @param path Path to the .rtf file
#' @param hide_data toggle to replace all data in tables with XX
#' @return List of page objects, each with:
#'   \item{parameter}{character or NA}
#'   \item{header}{block of header rows (see section 4)}
#'   \item{data}{block of data rows, with stable \code{row_id}s}
parse_rtf <- function(path, hide_data = FALSE) {
  if(!file.exists(path)){
    stop(err_rtf_unreadable(path, "file does not exist"))
  }
  if(dir.exists(path)){
    stop(err_rtf_unreadable(path, "path is a directory, not a file"))
  }


  text  <- tryCatch(
    rtf_read_raw(path),
    error = function(e) stop(err_rtf_unreadable(path,e))
  )

  pages <- tryCatch(
    rtf_split_pages(text),
    error = function(e) stop(err_rtf_parse_failed(path, "split_pages", e))
  )

  if(length(pages) == 0L){
    stop(err_rtf_parse_failed(
      path, "split_pages",
      "no table sections (\\sectd) were found in the file"
    ))
  }
  pages <- tryCatch(
    lapply(pages, function(p) parse_page(p, hide_data)),
    error = function(e) stop(err_rtf_parse_failed(path, "parse_page", e))
  )

  # Stable row identity: number data rows sequentially across all pages in
  # document order. Row exclusions are stored against these IDs rather than
  # view positions, so an exclusion keeps pointing at the same row when
  # parameter/timeline filters change the set of visible rows.
  next_id <- 1L
  for (pi in seq_along(pages)) {
    n <- block_nrow(pages[[pi]]$data)
    if (n > 0L) {
      pages[[pi]]$data$row_id <- seq.int(next_id, length.out = n)
      next_id <- next_id + n
    }
  }
  pages
}


#' Prepare a combined table with filtering and exclusions applied
#'
#' Common pipeline used by html.R and xml.R public functions.
#' @param pages Pre-parsed pages (from parse_rtf) or NULL
#' @param path RTF file path (used only if pages is NULL)
#' @param excluded_cols Integer vector of column indices to exclude
#' @param excluded_rows Integer vector of stable data-row IDs to exclude
#'   (as assigned by \code{parse_rtf}; equal to the row's position in the
#'   unfiltered combined table)
#' @param excluded_header_rows Integer vector of header row indices to exclude
#' @param parameters Parameter filter
#' @param timelines Timeline filter
#' @param levels Indent level filter
#' @param hide_data toggle to replace all data in tables with XX
#' @return list(combined, included_cols) where combined has rows filtered
prepare_table <- function(pages = NULL, path = NULL,
                          excluded_cols = NULL, excluded_rows = NULL,
                          excluded_header_rows = NULL,
                          parameters = NULL, timelines = NULL, levels = NULL,
                          hide_data = FALSE) {
  if (is.null(pages) || hide_data) pages <- parse_rtf(path, hide_data)

  param_filtered    <- filter_pages(pages, parameters)
  pages <- param_filtered$pages
  warnings <- param_filtered$warnings

  tl_filtered <- filter_timelines(combine_pages(pages),timelines)
  warnings <- c(warnings,tl_filtered$warnings)

  lvl_filtered <- filter_levels(tl_filtered$combined, levels)
  warnings <- c(warnings, lvl_filtered$warnings)

  combined <- remove_continuations(lvl_filtered$combined)

  n_cols <- length(combined$col_widths_twips)
  n_data <- block_nrow(combined$data)
  n_hdr  <- block_nrow(combined$header)

  inc_cols <- setdiff(seq_len(n_cols), excluded_cols        %||% integer())
  inc_hdrs <- setdiff(seq_len(n_hdr),  excluded_header_rows %||% integer())

  # Data rows are dropped by stable row ID, not by position in the (possibly
  # filtered) view; rows without an ID fall back to their current position
  row_ids <- combined$data$row_id
  row_ids[is.na(row_ids)] <- seq_len(n_data)[is.na(row_ids)]
  keep_rows <- !(row_ids %in% (excluded_rows %||% integer()))

  combined$data   <- block_rows(combined$data, keep_rows)
  combined$header <- block_rows(combined$header, inc_hdrs)

  list(combined = combined, included_cols = inc_cols, warns = warnings)
}



# --combining pages


#' Combine multiple pages into a single flat table
#'
#' Header rows are taken from the first page only.
#' Data rows are concatenated across all pages.
#' @param pages Filtered list of page objects
#' @return list(header, data, col_widths_twips) where header/data are blocks
combine_pages <- function(pages) {
  if (length(pages) == 0L) {
    return(list(header = block_new(), data = block_new(),
                col_widths_twips = numeric()))
  }

  header <- pages[[1]]$header
  footer <- pages[[1]]$footer
  data   <- block_rbind_all(lapply(pages, `[[`, "data"))
  data <- block_categorise(data)

  # Column widths from the first header (or data) row
  #ref <- if (block_nrow(header) > 0L) header else data
  ref <- data
  col_widths_twips <- if (block_nrow(ref) > 0L) {
    as.numeric(ref$width[1L,])
  } else {
    numeric()
  }

  list(
    header           = header,
    data             = data,
    footer           = footer,
    col_widths_twips = col_widths_twips
  )
}

# -- info function


#' Get summary info about an RTF file for the UI
#'
#' @param path Path to the .rtf file
#' @return list(n_cols, n_rows, col_names, parameters, timelines)
get_table_info <- function(path) {
  table_info_from_pages(parse_rtf(path))
}

#' Get summary info from already-parsed pages (used by app.R's cache)
#'
#' @param pages Output of parse_rtf()
#' @return list(n_cols, n_rows, col_names, parameters, timelines)
table_info_from_pages <- function(pages) {
  combined <- combine_pages(pages)

  ref <- if (block_nrow(combined$header) > 0L) {
    combined$header
  } else if (block_nrow(combined$data) > 0L) {
    combined$data
  } else {
    NULL
  }

  n_cols    <- if (!is.null(ref)) sum(ref$present[1L, ]) else 0L
  col_names <- if (!is.null(ref)) {
    ref$text[1L, ref$present[1L, ]]
  } else {
    character()
  }

  list(
    n_cols     = n_cols,
    n_rows     = block_nrow(combined$data),
    n_hdrs     = block_nrow(combined$header),
    col_names  = col_names,
    parameters = get_parameters(pages),
    timelines  = get_timelines(combined),
    levels     = names(get_levels(combined))
  )
}







# -- images

#' Check weather an RTF file contains an embedded image (PNG)
#'
#' @param path Path to the .rtf file
#' @return TRUE if a \\pngblip group is found, FALSE otherwise
is_image_rtf <- function(path) {
  # Read only enough to find the marker - expected format is just an image nothing else
  # Raw text scan suits this purpose
  text <- rtf_read_raw(path)
  grepl("\\\\pngblip(?![a-zA-Z])", text, perl = TRUE)
}



#' Extract the first PNG image from an RTF file
#'
#' Locates the first \code{\\{\\pict ... \\pngblip ... <hex>\\}} group, decodes the
#' hex-encoded bytes to raw, and returns the image data plus its declared
#' dimensions in twips (\code{\\picwgoal} / \code{\\pichgoal}).
#'
#' @param path Path to the .rtf file
#' @return list(png_bytes = raw, width_twips = integer, height_twips = integer)
#'   or NULL if no PNG found
extract_png <- function(path) {
  text <- rtf_read_raw(path)
  n    <- nchar(text)

  # Find the first \pict group that contains \pngblip. Earlier \pict groups
  # may be WMF/EMF renditions of the same image, so keep scanning past them.
  pict_text   <- NULL
  search_from <- 1L
  repeat {
    m <- regexpr("\\\\pict(?![a-zA-Z])", substr(text, search_from, n), perl = TRUE)
    if (m == -1L) return(NULL)
    pict_pos <- search_from + m[1L] - 1L

    # Walk back to opening brace
    brace_pos <- NA_integer_
    for (i in seq(pict_pos - 1L, max(1L, pict_pos - 5L), by = -1L)) {
      if (substr(text, i, i) == "{") { brace_pos <- i; break }
    }
    if (is.na(brace_pos)) { search_from <- pict_pos + 5L; next }

    end_pos <- find_matching_brace(text, brace_pos)
    if (is.na(end_pos)) return(NULL)

    candidate <- substr(text, brace_pos, end_pos)
    if (grepl("\\\\pngblip(?![a-zA-Z])", candidate, perl = TRUE)) {
      pict_text <- candidate
      break
    }
    search_from <- end_pos + 1L
  }

  # Drop {\*...} destination groups (e.g. {\*\blipuid <32 hex chars>}) so
  # their hex payload can't be mistaken for image data below
  pict_text <- gsub("\\{\\\\\\*[^{}]*\\}", "", pict_text, perl = TRUE)

  # Extract \picwgoal and \pichgoal
  w_match <- regmatches(pict_text, regexpr("\\\\picwgoal([0-9]+)", pict_text, perl = TRUE))
  h_match <- regmatches(pict_text, regexpr("\\\\pichgoal([0-9]+)", pict_text, perl = TRUE))
  width_twips  <- if (length(w_match) > 0L) as.integer(sub("\\\\picwgoal", "", w_match)) else NA_integer_
  height_twips <- if (length(h_match) > 0L) as.integer(sub("\\\\pichgoal", "", h_match)) else NA_integer_

  # Extract the hex data: it starts on the line after the last RTF control word.
  # RTF control words are \word or \wordN; the hex blob follows on its own line(s).
  # Strategy: find the position right after the last \controlword (with optional
  # numeric argument) in the pict group, then take the rest up to the closing brace.
  last_ctrl <- gregexpr("\\\\[a-zA-Z]+[-]?[0-9]*", pict_text, perl = TRUE)[[1]]
  if (identical(last_ctrl, -1L)) return(NULL)
  last_end <- last_ctrl[length(last_ctrl)] +
    attr(last_ctrl, "match.length")[length(last_ctrl)] - 1L
  hex_region <- substr(pict_text, last_end + 1L, nchar(pict_text))
  # Keep only hex characters, strip everything else (whitespace, braces)
  hex_clean <- gsub("[^0-9a-fA-F]", "", hex_region, perl = TRUE)
  if (nchar(hex_clean) == 0L) return(NULL)
  if (nchar(hex_clean) %% 2L != 0L) return(NULL)
  png_bytes <- hex_to_raw(hex_clean)

  list(
    png_bytes    = png_bytes,
    width_twips  = width_twips,
    height_twips = height_twips
  )
}

