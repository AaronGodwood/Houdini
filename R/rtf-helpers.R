#' Splits up pages of an RTF document
#'
#' @param rtf raw rtf markup to be split
#' @param filters any parameter filter to filter out certain pages
#'
#' @return a list of rtf markup split by page
#' @importFrom stringr str_extract
#' @keywords internal
#'
get_pages <- function(rtf,filters){

  parameters <- filters$parameters
  rtf <- rtf %>%
    strsplit(split = "\\\\sectd") %>%
    unlist()
  rtf <- rtf[-1]

  headers <- rtf %>%
    lapply(function(x) get_header(x))
  footers <- rtf %>%
    lapply(function(x) get_footer(x))
  new_headers <- list()
  new_footers <- list()
  new_rtf <- c()
  for(j in seq_along(parameters)){
    for(i in seq_along(headers)){
      if(grepl(parameters[j],headers[[i]]$parameter,fixed = TRUE)){
        new_headers <- append(new_headers,headers[i])
        new_footers <- append(new_footers,footers[i])
        new_rtf <- c(new_rtf,rtf[i])
      }
    }
  }
  if(length(new_rtf) == 0){
    new_rtf <- rtf
    new_headers <- headers
    new_footers <- footers
  }


  new_rtf <- new_rtf %>%
    lapply(function(x) {
      x <- x %>%
        str_extract("(?s)(?<=\\{\\\\\\*\\\\bkmkend IDX[0-9]?[0-9]?[0-9]?\\}).*") %>%
        strsplit("\\{\\\\row\\}\\\r\\\n") %>%
        unlist()
      x[-length(x)]
    }) %>%
    unname()
  new_rtf
}


#' Gets the header section of an rtf markup page
#'
#' @param rtf_page the rtf markup page
#'
#' @return a header section in rtf markup
#' @importFrom stringr str_extract
#' @keywords internal
#'
get_header <- function(rtf_page){
  header <- rtf_page %>%
    str_extract("(?s)(\\{\\\\header)(.*?)(\\{\\\\par\\}\\}\\})") %>%
    str_extract("(?s)(\\\\trowd)(.*)") %>%
    strsplit("\\{\\\\row\\}\\\r\\\n") %>%
    unlist() %>%
    lapply(function(x) extract_rtf_row(x)) %>%
    unname()
  header <- header[-length(header)]

  out <- tryCatch(
    {
      list(
        cut = header[[2]]$texts[2],
        run = header[[2]]$texts[1],
        tableid = header[[3]]$texts,
        title = header[[4]]$texts,
        analysis = header[[5]]$texts,
        parameter = header[[6]]$texts
      )
    },
    error = function(e){
      list(
        cut = header[[2]]$texts[2],
        run = header[[2]]$texts[1],
        tableid = header[[3]]$texts,
        title = header[[4]]$texts,
        analysis = header[[5]]$texts,
        parameter = c()
      )
    }
  )

  class(out) <- "header"
  out
}


#' Gets the footer section of an rtf markup page
#'
#' @param rtf_page the rtf markup page
#'
#' @return a footer section in rtf markup
#' @importFrom stringr str_extract
#' @keywords internal
#'
get_footer <- function(rtf_page){
  footer <- rtf_page %>%
    str_extract("(?s)(\\{\\\\footer)(.*?)(\\\\pard\\}\\})") %>%
    str_extract("(?s)(\\\\trowd)(.*)") %>%
    strsplit("\\{\\\\row\\}\\\r\\\n") %>%
    unlist() %>%
    lapply(function(x) extract_rtf_row(x)) %>%
    unname()
  footer <- footer[-length(footer)]
  footnotes <- c()
  for(i in seq_along(footer)){
    if(footer[[i]]$texts == "")
    {
      break
    }
    else{
      footnotes <- c(footnotes,footer[[i]]$texts)
    }
  }
  out <- list(
    footnotes = footnotes,
    info = footer[[length(footer)]]$texts
  )
  class(out) <- "footer"
  out
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
