#' @useDynLib Houdini, .registration = TRUE
#' @keywords internal
"_PACKAGE"


# Null-coalescing helper shared across the package.
`%||%` <- function(a, b) if (!is.null(a)) a else b

## usethis namespace: start
#' @importFrom purrr is_empty
## usethis namespace: end
NULL
