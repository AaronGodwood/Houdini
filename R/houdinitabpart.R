
houdini_tabpart <- function(data, col_keys = names(data),
                            cwidth = NULL, cheight = NULL) {

  span_init <- matrix(1L, nrow = nrow(data), ncol = length(col_keys))

  spans <- list(rows = span_init)


  if (length(cwidth) == length(col_keys)) {
    colwidths <- cwidth
  } else {
    colwidths <- rep(cwidth, length(col_keys))
  }

  rowheights <- rep(cheight, nrow(data))

  out <- list(
    dataset = data,
    col_keys = col_keys,
    colwidths = colwidths,
    rowheights = rowheights,
    hrule = rep("auto", nrow(data)),
    spans = spans
  )
  class(out) <- "houdini_tabpart"
  out
}



