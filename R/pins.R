

#' Pins a folder of RTF files to POSIT Connect
#'
#' @param path the file path to the folder of .RTF files to pin
#' @param name the name the pin will take on POSIT Connect
#'
#' @return The fully qualified name of the new pin invisibly
#' @export
houdini_pin_folder <- function(path, name){
  tryCatch(
    board <- pins::board_connect(),
    error = function(e){
      message("Cannot connect to board")
    }
  )
  if(!dir.exists(path)){
    stop(houdini_error(
      "rtf_folder_missing",
      paste0("RTF folder not found: ", path),
      "Check the path to the folder holding the rtf files"
    ))
  }
  pins::pin_upload(
    board,
    paths = list.files(
      path,
      full.names = TRUE
    ),
    name = name
  )
}


