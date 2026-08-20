

#' Pins a folder of RTF files to POSIT Connect
#'
#' @param path the file path to the folder of .RTF files to pin
#' @param name the name the pin will take on POSIT Connect
#'
#' @return The fully qualified name of the new pin invisibly
#' @export
houdini_pin_folder <- function(path, name){

  if(!dir.exists(path)){
    stop(houdini_error(
      "rtf_folder_missing",
      paste0("RTF folder not found: ", path),
      "Check the path to the folder holding the rtf files",
      list(path = path)
    ))
  }

  files <- list.files(path, full.names = TRUE)
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


