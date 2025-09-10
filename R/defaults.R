houdini_global <- new.env(parent = emptyenv())

houdini_default_settings <- list(
  rowlbls.name = "ROWLBL",
  cols.name = "COL",
  colvars.name = "COLVAR",
  cellvalcs.name = "CELLVALC",
  trtlbls.name = "TRTLBL",
  param.name = "BYLBL",
  rowgrp.name = "ROWORD",
  delimiter = "\\^\\*\\^",
  delimiter.non.regex = "^*^") #maybe will change this to come from previous definition

houdini_global$bookmark_jmptbl <- list()

houdini_global$defaults <- houdini_default_settings


#' Sets the default options for the Houdini package
#'
#' @param rowlbls.name the name that the columns containing row labels/descriptors are called in standard data sets
#' @param cols.name the name that the data columns are called in standard data sets
#' @param colvars.name the name of the columns containing data column headers in non-standard data sets
#' @param cellvalcs.name the name of the column containing data points in non-standard data sets
#' @param trtlbls.name the name of the column containing a second layer of headers for some tables
#' @param param.name the name of the column containing the data parameter filtering is done by
#' @param rowgrp.name the name of the column describing the row group
#' @param delimiter the split delimiter used for splitting header rows and footer rows (IMPORTANT: use regex escape characters (\\) infront of special characters)
#' @param delimiter.non.regex the split delimiter used for splitting header rows and footer rows without regex escape characters
#'
#' @return invisible
#' @importFrom utils modifyList
#' @export
#'
set_houdini_defaults <- function(
    rowlbls.name = NULL,
    cols.name = NULL,
    colvars.name = NULL,
    cellvalcs.name = NULL,
    trtlbls.name = NULL,
    param.name = NULL,
    rowgrp.name = NULL,
    delimiter = NULL,
    delimiter.non.regex = NULL){

  x <- list()

  if(!is.null(rowlbls.name)){
    x$rowlbls.name <- rowlbls.name
  }

  if(!is.null(cols.name)){
    x$cols.name <- cols.name
  }

  if(!is.null(colvars.name)){
    x$colvars.name <- colvars.name
  }

  if(!is.null(cellvalcs.name)){
    x$cellvalcs.name <- cellvalcs.name
  }

  if(!is.null(trtlbls.name)){
    x$trtlbls.name <- trtlbls.name
  }

  if(!is.null(param.name)){
    x$param.name <- param.name
  }

  if(!is.null(rowgrp.name)){
    x$rowgrp.name <- rowgrp.name
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

#' Restores Houdini package options to the defaults
#'
#' @return invisible
#' @export
#'
init_houdini_defaults <- function(){
  x <- houdini_default_settings
  houdini_global$defaults <- x
  class(x) <- "houdini_defaults"
  invisible(x)
}


#' Returns the default settings for the Houdini package
#'
#' @return defalt settings of the houdini package
#' @export
#'
get_houdini_defaults <- function(){
  x <- houdini_default_settings
  class(x) <- "houdini_defaults"
  x
}
