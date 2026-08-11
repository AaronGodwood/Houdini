#' Launch the Houdini shiny app
#'
#' Opens the Houdini app in your default browser
#'
#' @param ... arguments passes to \code{\link[shiny]{runApp}} e.g.
#' \code{port}, \code{launch.browser}.
#' @return Called for its side effects of launching the app
#' @export
run_app <- function(...) {
  shiny::runApp(houdini_app(), ...)
}
