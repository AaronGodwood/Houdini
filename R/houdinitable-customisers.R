#' Adds footer rows to a houdinitable object
#'
#' @param ht a houdinitable object for which you want to add footers
#' @param footers a character vector of vectors to be added as footnotes
#'
#' @return a houidnitbale object with footers added
#' @export
#'
#' @examples
add_footer <- function(ht,footers){
  #changes nothing if there are no footers
  if(purrr::is_empty(footers)){
    return(ht)
  }
  ht$footer$dataset <- ht$footer$dataset %>%
    sapply(function(x){
      c(x,footers)
    }) %>%
    data.frame()
  spans <- data[FALSE, , drop = FALSE]
  for(i in 1:nrow(ht$footer$dataset)){
    spans[i,] = get_runs(ht$footer$dataset[i,])
  }
  ht$footer$spans <- spans %>%
    lapply(as.numeric) %>%
    data.frame()
  ht
}


#' Sets column text alignments for a houdinitable object
#'
#' @param ht a houdinitable object for which you want to set the column alignments
#' @param alignment a character representing an alignment that you want to set the columns to. Either "start", "center" or "end", defaults to "start"
#' @param col_keys column keys defining the columns you want to set alignments of
#'
#' @return a houdinitable object with column text alignments changed
#' @export
#'
#' @examples
set_alignments <- function(ht,alignment, col_keys = NULL){
  if(is.null(col_keys)){
    alignments <- rep(alignment,length(ht$alignments))
    names(alignments) <- names(ht$alignments)
    ht$alignments <- alignments
    return(ht)
  }

  if(any(sapply(col_keys,function(x){!(x %in% names(ht$alignments))}))){
    stop("Col keys are not all present in houidnitable")
  }
  alignment <- match.arg(alignment, c("start", "center", "end"), several.ok = FALSE)
  for(i in seq_along(col_keys)){
    ht$alignments[[col_keys[i]]] <- alignment
  }
  return(ht)

}

#' Sets column widths for a houdinitable object
#'
#' @param ht a houdinitable object for which you want to set the column alignments
#' @param width a float or integer defining the width you wish to set the specified column to
#' @param col_keys column keys defining the columns you want to set the widths of
#'
#' @return
#' @export
#'
#' @examples
set_widths <- function(ht,width, col_keys = NULL){
  if(is.null(col_keys)){
    widths <- rep(width,length(ht$widths))
    names(widths) <- names(ht$widths)
    ht$widths <- widths
    return(ht)
  }

  if(any(sapply(col_keys,function(x){!(x %in% names(ht$widths))}))){
    stop("Col keys are not all present in houidnitable")
  }
  for(i in seq_along(col_keys)){
    ht$widths[[col_keys[i]]] <- width
  }
  return(ht)

}


#' Stops groups of data being split over a page in a word doc
#'
#' @param ht a houdinitable object for which you want data to be kept together
#' @param column a list of data that defines the groups of data within a table
#'
#' @return a houdinitable object with data grouped
#' @export
#'
#' @examples
paginate <- function(ht, column){
  if(length(column) < 2)
  {
    return(ht)
  }
  keep_with_next <- rep(TRUE,length(column))
  for(i in 1:(length(column)-1)){
    if(!is.na(column[i]) && !is.na(column[i+1])){
      if(column[i] != column[i+1])
      {
        keep_with_next[i] <- FALSE
      }
    }
  }
  ht$keep_with_next <- keep_with_next
  ht
}
