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
  for(i in seq_along(col_keys)){
    ht$alignments[[col_keys[i]]] <- alignment
  }
  return(ht)

}

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
