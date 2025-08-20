houdini_global <- new.env(parent = emptyenv())

houdini_default_settings <- list(
  rowlbls.name = "ROWLBL",
  cols.name = "COL",
  colvars.name = "COLVAR",
  cellvalcs.name = "CELLVALC",
  delimiter = "\\^\\*\\^",
  delimiter.non.regex = "^*^") #maybe will change this to come from previous definition

houdini_global$defaults <- houdini_default_settings


set_houdini_defaults <- function(
    rowlbls.name = NULL,
    cols.name = NULL,
    colvars.name = NULL,
    cellvalcs.name = NULL,
    delimiter = NULL,
    delimiter.non.regex = NULL){

  x <- list()

  if(!is.null(rowlbls.name)){
    x$rowlbls.name <- rowlbl.name
  }

  if(!is.null(cols.name)){
    x$cols.name <- cols.name
  }

  if(!is.null(colvars.name)){
    x$colvars.name <- colvars.name
  }

  if(!is.null(cellvalcs.name)){
    x$colvalcs.name <- colvalcs.name
  }

  if(!is.null(delimiter)){
    x$delimiter <- delimiter
  }

  if(!is.null(delimiter.non.regex)){
    x$delimiter.non.regex <- delimiter.non.regex
  }

  houdini_defaults <- houdini_global$defaults
  houdini_new_defaults <- modifyList(houdini_defaults, x)
  houdini_global$defaults <- houdini_new_defaults
  invisible(x)
}

init_houdini_defaults <- function(){
  x <- houdini_default_settings
  houdini_global$defaults <- x
  class(x) <- "houdini_defaults"
  invisible(x)
}

get_houdini_defaults <- function(){
  x <- houdini_default_settings
  class(x) <- "houdini_defaults"
  x
}
