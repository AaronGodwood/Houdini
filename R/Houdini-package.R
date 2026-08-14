#' @useDynLib Houdini, .registration = TRUE
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils unzip adist
#' @importFrom xml2 read_xml write_xml xml_find_all xml_find_first xml_attr xml_name xml_parent xml_add_child xml_add_sibling xml_root xml_path xml_remove xml_child xml_replace xml_text `xml_attr<-`
"_PACKAGE"


# Null-coalescing helper shared across the package.
`%||%` <- function(a, b) if (!is.null(a)) a else b

## usethis namespace: start
#' @importFrom purrr is_empty
## usethis namespace: end
NULL
