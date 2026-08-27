

#' Pins a folder of RTF files to POSIT Connect
#'
#' @param path the file path to the folder of .RTF files to pin
#' @param figure_path a second optional folder path for when figures are stored separately from tables
#' @param name the name the pin will take on POSIT Connect
#'
#' @return The fully qualified name of the new pin invisibly
#' @export
houdini_pin_folder <- function(path, figure_path = NULL, name){

  if(!dir.exists(path)){
    stop(houdini_error(
      "rtf_folder_missing",
      paste0("RTF folder not found: ", path),
      "Check the path to the folder holding the rtf files",
      list(path = path)
    ))
  }

  files <- list.files(path ,pattern = "\\.rtf$", ignore.case = TRUE, full.names = TRUE)

  same_folder <- is.null(figure_path) ||
    normalizePath(figure_path, winslash = "/", mustWork = FALSE) ==
    normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!same_folder){

    if(!dir.exists(figure_path)){
      stop(houdini_error(
        "rtf_folder_missing",
        paste0("RTF folder not found: ", figure_path),
        "Check the path to the folder holding the rtf files",
        list(path = path)
      ))
    }

    figures <- list.files(figure_path, pattern = "\\.rtf$", ignore.case = TRUE, full.names = TRUE)
    clashes <- intersect(names(files), names(figures))
    if (length(clashes)) {
      warning(sprintf(
        "%d name%s present in both RTF folders; using the copy in %s: %s",
        length(clashes), if (length(clashes) == 1L) "" else "s",
        file_location, paste(clashes, collapse = ", ")
      ), call. = FALSE)
    }

    files <- c(files, figures[setdiff(figures, files)])
  }


  if(length(files) == 0L){
    stop(houdini_error(
      "rtf_folder_missing",
      paste0("No files to pin in: ", path),
      "Check the folder holds the .rtf files you meant to upload",
      list(path = path)
    ))
  }

  board <- tryCatch(
    pins::board_connect(),
    error = function(e){
      stop(houdini_error(
        "connect_unreachable",
        paste0("Cannot connect to Posit Connect ", conditionMessage(e)),
        "Check CONNECT_SERVER and CONNECT_API_KEY, and that the server is reachable",
        list(cause = conditionMessage(e))
      ))
    }
  )

  invisible(pins::pin_upload(board, paths = files, name = name))
}


