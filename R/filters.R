
# --filtering



#' Resolve column specification to integer indices
#'
#' Handles NULL (all), integer vector, or character names matched against headers.
#' @param cols Column spec (NULL, integer vector, or character names)
#' @param n_cols_total Total number of columns
#' @param header Header block (for name matching)
#' @return Integer vector of valid 1-based column indices
resolve_cols <- function(cols, n_cols_total, header) {
  if (is.null(cols) || length(cols) == 0L) return(seq_len(n_cols_total))

  cols_int <- suppressWarnings(as.integer(cols))
  if (anyNA(cols_int)) {
    ref_names <- if (block_nrow(header) > 0L) {
      header$text[block_nrow(header), ]
    } else character()
    cols_int <- match(as.character(cols), ref_names)
    cols_int <- cols_int[!is.na(cols_int)]
  }
  cols_int <- cols_int[cols_int >= 1L & cols_int <= n_cols_total]
  if (length(cols_int) == 0L) seq_len(n_cols_total) else cols_int
}


#' Resolve a start/end row range to integer indices
#'
#' @param n Number of rows available
#' @param row_start 1-based start (NULL = 1)
#' @param row_end 1-based end (NULL = last)
#' @return Integer vector of row indices (possibly empty)
slice_range <- function(n, row_start = NULL, row_end = NULL) {
  if (n == 0L) return(integer())
  rs <- if (is.null(row_start)) 1L else max(1L, as.integer(row_start))
  re <- if (is.null(row_end))   n   else min(n, as.integer(row_end))
  if (rs <= re) seq.int(rs, re) else integer()
}


#' Filter pages by parameter value
#'
#' Pages with NA parameter are always kept.
#' @param pages Output of parse_rtf()
#' @param parameters Character vector of parameter values to include
filter_pages <- function(pages, parameters) {
  if (is.null(parameters) || length(parameters) == 0L) return(list(pages = pages, warnings = list()))

  #gets parameters present in the table and only applies those filters
  page_params <- get_parameters(pages)
  present_params <-  parameters[parameters %in% page_params]

  #warns about not found filters for logging purposes
  not_present <- parameters[!parameters %in% page_params]
  warnings <- lapply(not_present, function(p) warn_filter_not_found(p,"Parameter"))

  if (is.null(present_params) || length(present_params) == 0L) return(list(pages = pages, warnings = warnings))



  list(pages = pages[vapply(pages, function(p) {
    is.na(p$parameter) || p$parameter %in% present_params
  }, logical(1))], warnings = warnings)
}

#' Filter rows by timeline value
#'
#' Rows with NA timeline are always kept.
#' @param combined Output of combine_pages()
#' @param timelines Character vector of timeline values to include
filter_timelines <- function(combined, timelines) {
  if (is.null(timelines) || length(timelines) == 0L) return(list(combined = combined, warnings = list()))

  #gets parameters present in the table and only applies those filters
  combined_tls <- get_timelines(combined)
  present_tls <-  timelines[timelines %in% combined_tls]

  #warns about not found filters for logging purposes
  not_present <- timelines[!timelines %in% combined_tls]
  warnings <- lapply(not_present, function(p) warn_filter_not_found(p,"Timepoint"))

  if (is.null(present_tls) || length(present_tls) == 0L) return(list(combined = combined, warnings = warnings))

  #gets first column where timepoint lables are present if they exist
  col1 <- block_cols(combined$data,1)$text

  labels <- col1

  labels[labels == ""] <- NA
  non_na <- labels[!is.na(labels)]
  idx <- cumsum(!is.na(labels))
  filled <- c(NA_character_,non_na)[idx+1]

  keep <- filled %in% present_tls | is.na(filled)
  rows <- which(keep)
  combined$data <- block_rows(combined$data,rows)
  list(combined = combined, warnings = warnings)
}

#' Filter rows by indent level value
#'
#' Rows with no content are always kept.
#' @param combined Output of combine_pages()
#' @param timelines Character vector of timeline values to include
filter_levels <- function(combined, levels){
  if (is.null(levels) || length(levels) == 0L) return(list(combined = combined, warnings = list()))

  combined_levels <- get_levels(combined)
  present_levels <- levels[levels %in% names(combined_levels)]

  not_present <- levels[!levels %in% names(combined_levels)]
  warnings <- lapply(not_present, function(p) warn_filter_not_found(p,"Level"))

  if (is.null(present_levels) || length(present_levels) == 0L) return(list(combined = combined, warnings = warnings))

  col1 <- block_cols(combined$data, 1)$text
  indents <- vapply(col1, function(t){
    if(t == "") return(NA_integer_)
    match <- regexpr("^\\s+",t)
    if(match[1] == -1) return(0)
    attr(match, "match.length")
  }, numeric(1))

  keep <- indents %in% combined_levels[[levels]] | is.na(indents)
  rows <- which(keep)
  combined$data <- block_rows(combined$data,rows)
  list(combined = combined, warnings = warnings)
}




#' Get all unique parameter values across pages
#'
#' @param pages Output of parse_rtf()
#' @return Character vector of unique parameter values
get_parameters <- function(pages) {
  params <- vapply(pages, `[[`, character(1), "parameter")
  unique(params[!is.na(params)])
}

#' Get all unique timeline values across pages
#'
#' @param combined Output of combine_pages()
#' @return Character vector of unique timeline values
get_timelines <- function(combined) {
  col1 <- block_cols(combined$data, 1)$text
  timelines <- col1[grepl("^Week\\s[0-9]+$",col1, perl = TRUE) | grepl("^Baseline$",col1, perl = TRUE)]
  unique(timelines[!is.na(timelines)])
}

#' Get all data/indent levels across pages
#'
#' @param combined Output of combine_pages()
#' @return Named list of indent count named by corresponding level
get_levels <- function(combined){
  hdr1 <- block_cols(combined$header, 1)$text[block_nrow(combined$header)]
  lines <- strsplit (hdr1, "\n")[[1]]
  hdr_indents <- vapply(lines, function(t){
    match <- regexpr("^\\s+",t)
    if(match[1] == -1) return(0)
    attr(match, "match.length")
  }, numeric(1))
  return(setNames(hdr_indents, trimws(lines)))
}

