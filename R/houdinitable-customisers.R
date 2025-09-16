#' Adds footer rows to a houdinitable object
#'
#' @param ht a houdinitable object for which you want to add footers
#' @param footers a character vector of vectors to be added as footnotes
#'
#' @return a houdinitable object with footers added
#' @importFrom purrr is_empty
#' @export
#'
add_footer <- function(ht,footers){
  #changes nothing if there are no footers
  if(purrr::is_empty(footers)){
    return(ht)
  }
  #adds footers to current footer df
  ht$footer$dataset <- ht$footer$dataset %>%
    sapply(function(x){
      c(x,footers)
    }) %>%
    t() %>%
    data.frame()
  #gets spans of current footers
  spans <- ht$footer$dataset[FALSE, , drop = FALSE]
  for(i in 1:nrow(ht$footer$dataset)){
    spans[i,] = get_runs(ht$footer$dataset[i,])
  }
  #makes sure the footers are integers and adds them to ht
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
set_alignments <- function(ht,alignment, col_keys = NULL){
  #if no columns specified add alignments to all columns
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
  #adds alignments to specified cols
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
#' @return a houdinitable object with modified widths
#' @export
#'
set_widths <- function(ht,width, col_keys = NULL){
  #if columns are not specified set width of all cols
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

#' Automatically calculates and sets widths of ht cols based on content
#'
#' @param ht a houdinitable object to calculate column widths for
#' @param data_cols a string vector of col names that all need to be the same width
#'
#' @return a houdinitable object with autocalculated widths
#' @export
#'
auto_widths <- function(ht, data_cols){
  all_parts <- rbind(ht$header$dataset,ht$body$dataset)
  all_parts <- rbind(ht$header$dataset,all_parts)
  all_parts <- all_parts %>%
    apply(c(1,2),function(x) nchar(x))
  all_parts[all_parts == 0] <- NA
  parts_length <- colMeans(all_parts, na.rm = TRUE) %>%
    sapply(function(x){
      if(is.na(x)){
        return(0)
      }
      return(x)
    })
  data_cols_length <- mean(parts_length[data_cols])
  parts_length <- names(parts_length) %>%
    sapply(function(x){
      if(x %in% data_cols)
      {
        return(data_cols_length)
      }
      parts_length[[x]]
    })
  total <- sum(parts_length)
  scale <- 10000 / total
  widths <- parts_length * scale
  ht$widths <- widths
  ht
}

#' Stops groups of data being split over a page in a word doc - not actually needed anymore - left in as keepwithnext is disabled in xml compilation
#'
#' @param ht a houdinitable object for which you want data to be kept together
#' @param column a list of data that defines the groups of data within a table
#'
#' @return a houdinitable object with data grouped
#' @export
#'
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
