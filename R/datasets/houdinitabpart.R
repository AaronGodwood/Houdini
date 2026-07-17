#' Creates a section (used for header, body, footer) of a houdinitable
#' @keywords internal
houdini_tabpart <- function(data, col_keys = names(data),
                            cwidth = NULL, cheight = NULL) {

  span_init <- matrix(1L, nrow = nrow(data), ncol = length(col_keys))

  spans <- data.frame(span_init)


  out <- list(
    dataset = data,
    col_keys = col_keys,
    hrule = rep("auto", nrow(data)),
    spans = spans
  )
  class(out) <- "houdini_tabpart"
  out
}



