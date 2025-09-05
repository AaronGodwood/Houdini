read_raw_rtf <- function(file_path){
  size <- file.info(file_path)$size

  readChar(file_path, size, useBytes = TRUE)
}




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

build_table <- function(raw_rtf, hide_data = FALSE){
  rtf_pages <- get_pages(raw_rtf)
  table_properties <- generate_table_properties(width = 1, layout = "fixed") # will access width and layout from ht
  #filter
  xml_rows <- rtf_pages %>%
    unlist() %>%
    compile_rows(hide_data)
  paste0("<w:tbl>",table_properties,xml_rows,"</w:tbl>")
}


compile_rows <- function(rtf_rows, hide_data = FALSE){
  rows <- rtf_rows %>%
    lapply(function(x) extract_rtf_row(x)) %>%
    unname()
  found_header <- FALSE
  max_widths <- get_max_widths(rows)
  max_ncells <- length(max_widths)
  xml_grid <- max_widths %>%
    get_standard_widths() %>%
    generate_xml_grid()
  xml_rows <- character()
  prev_ncells <- 0
  for(i in seq_along(rows)){
    row <- rows[[i]]
    if(row$ncells == 1 && prev_ncells == 1)
    {
      next
    }
    spans <- get_rtf_spans(max_widths, row$widths)
    row$texts <- row$texts %>%
      normalise_texts(spans)
    row$alignments <- row$alignments %>%
      normalise_texts(spans)
    if(!row$header){
      found_header <- TRUE
    }

    if(i != length(rows) && rows[[i+1]]$ncells == 1){
      keep_with_next <- FALSE
    }else{
      keep_with_next <- TRUE
    }

    if(row$header){
      if(found_header){
        next
      }else if(row$ncells == max_ncells){
        xml_rows <- c(xml_rows,generate_xml_row(row$texts,bold = TRUE, alignment = row$alignments, part = "header", header = 1, keep_with_next = TRUE, spans = spans, hide_data = hide_data))
      }else{
        xml_rows <- c(xml_rows,generate_xml_row(row$texts,bold = TRUE, alignment = row$alignments, part = "header", header = 2, keep_with_next = TRUE, spans = spans, hide_data = hide_data))
      }
    }else{
      xml_rows <- c(xml_rows,generate_xml_row(row$texts, alignment = row$alignments, part = "body", keep_with_next = keep_with_next, spans = spans, hide_data = hide_data))
    }
    prev_ncells <- row$ncells

  }
  xml_rows <- paste(xml_rows, collapse = "")
  paste0(xml_grid,xml_rows)
}

extract_rtf_row <- function(row){
  sections <- row %>%
    strsplit(split = "\\\r\\\n\\\\") %>%
    unlist()
  ncells <- ((length(sections)+1)/2)-1
  header <- grepl("\\\\trhdr",sections[1])
  widths <- integer(ncells)
  alignments <- character(ncells)
  texts <- character(ncells)
  for(i in seq_len(ncells)){
    cell_brdrs <- sections[i+1]
    cell_prop <- sections[i+1+ncells]
    widths[i] <- as.integer(str_extract(cell_brdrs,"(?<=\\\\cellx).*"))
    alignments[i] <- str_match(cell_prop,"\\\\q(.*?)\\\\f1")[,2]
    texts[i] <- extract_text(cell_prop)
  }
  alignments <- alignments %>%
    sapply(function(x){
      if(is.na(x)){
        return("start")
      }
      if(x == "c"){
        return("center")
      }
      else if(x == "r"){
        return("end")
      }
      else{
        return("start")
      }
    })
  out <- list(
    widths = widths,
    alignments = alignments,
    texts = texts,
    ncells = ncells,
    header = header
  )
  out
}



extract_text <- function(cell_prop){
  cell_prop <- cell_prop %>%
    gsub("\\\r\\\n","",.)
  text <- str_match(cell_prop,"\\{(.*?)\\\\cell\\}")[,2]
  if(grepl("^\\\\",text)){
    text <- text %>%
      gsub("^[^ ]* ","",.)
  }
  text <- text %>%
    gsub("\\{\\\\line\\}","\n",.)
  if(is.na(text)){
    return("")
  }
  return(text)
}
