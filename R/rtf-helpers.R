get_pages <- function(rtf){
  rtf <- rtf %>%
    strsplit(split = "\\\\sectd") %>%
    unlist()
  rtf <- rtf %>%
    sapply(function(x) {
      x <- x %>%
        str_extract("(?s)(?<=\\{\\\\\\*\\\\bkmkend IDX[0-9]?[0-9]?[0-9]?\\}).*") %>%
        strsplit("\\{\\\\row\\}\\\r\\\n") %>%
        unlist()
      x[-length(x)]
    }) %>%
    unname()
  rtf[-1]
}


get_rtf_spans <- function(max_widths, widths){
  prev_span <- 0
  spans <- c()
  for(i in seq_along(widths)){
    span <- which(max_widths == widths[i]) - prev_span
    spans <- c(spans,span,rep(0,span-1))
    prev_span <- prev_span + span
  }
  spans
}

get_max_widths <- function(rows){
  max_ncells <- 0
  max_widths <- c()
  for(i in seq_along(rows)){
    row <- rows[[i]]
    if(row$ncells > max_ncells){
      max_ncells <- row$ncells
      max_widths <- row$widths
    }
  }
  max_widths
}

get_standard_widths <- function(max_widths){
  widths <- integer(length(max_widths))
  for(i in seq_along(max_widths)){
    if(i == 1)
    {
      widths[i] <- max_widths [i]
    }
    else{
      widths[i] <- max_widths[i] - max_widths[i-1]
    }
  }
  widths
}

normalise_texts <- function(texts,spans){
  if(length(texts) == length(spans)){
    return(texts)
  }
  span_count <- 0
  new_texts <- character()
  for(i in seq_along(spans)){
    if(spans[i] != 0){
      span_count <- span_count + 1
      new_texts <- c(new_texts,rep(texts[span_count],spans[i]))
    }
  }
  new_texts
}
