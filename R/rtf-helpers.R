#' Splits up pages of an RTF document and filters based on header
#'
#' @param rtf raw rtf markup to be split
#' @param filters any parameter filter to filter out certain pages
#'
#' @return a list of rtf markup split by page
#' @importFrom stringr str_extract
#' @keywords internal
#'
get_pages <- function(rtf,filters){


  rtf <- rtf %>%
    strsplit(split = "\\\\sectd") %>%
    unlist()
  rtf <- rtf[-1]

  headers <- rtf %>%
    lapply(function(x) get_header(x))
  footers <- rtf %>%
    lapply(function(x) get_footer(x))

  page_parameters <- headers %>%
    sapply(function(x){
      return(x$parameter)
    }) %>%
    unlist()

  parameters <- filters$parameters %>%
    sapply(function(x){
      # x <- gsub("([\\+\\*\\?\\[\\]\\(\\)\\{\\}\\|\\.\\^\\$])", "\\\\\\1", x)
      # x <- paste0("(^|\\s)",x)
      if(x %in% page_parameters)
      {
        x
      }
      else{
        logger::log_warn("Parameter filter: {x} not found - parameter filter not applied", namespace = "Houdini Log")
        c()
      }
    }) %>%
    unlist()
  parameters <- parameters[!is.null(parameters)]
  for(i in seq_along(parameters)){
    logger::log_info("Parameter filter: {parameters[i]} applied", namespace = "Houdini Log")
  }



  new_headers <- list()
  new_footers <- list()
  new_rtf <- c()

  for(i in seq_along(headers)){
    if(length(headers[[i]]$parameter) > 0 && headers[[i]]$parameter %in% parameters){
      new_headers <- append(new_headers,headers[i])
      new_footers <- append(new_footers,footers[i])
      new_rtf <- c(new_rtf,rtf[i])
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
  out <- list(
    rtf = new_rtf,
    header = headers[[1]],
    footer = footers[[1]]
  )

  out
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
  header <- header[sapply(header,function(x) any(x$texts != ""))]

  out <- tryCatch(
    {
      list(
        cut = header[[2]]$texts[2],
        run = header[[2]]$texts[1],
        tableid = header[[3]]$texts,
        title = header[[4]]$texts,
        analysis = header[[5]]$texts,
        parameter = stringr::str_extract(header[[6]]$texts, "(?<=:\\s).*")
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
  if(length(footer) > 1){
    for(i in seq_along(footer)){
      if(footer[[i]]$texts == "" || footer[[i]]$texts == " " )
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
  }else{
    out <- list(
      footnotes = c(),
      info = footer[[1]]$texts
    )
  }

  class(out) <- "footer"
  out
}

#' Gets the spans ( how may columns each cell spans) of a row based on the widths of its cells
#' @keywords internal
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

#' Gets the widths of a row with the max number of cells
#' @keywords internal
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

#' Gets widths as non-cumulative values
#' @keywords internal
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

#' Makes sure the texts attribute of a row object spans all columns of a table
#' @keywords internal
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
