read_raw_rtf <- function(file_path){
  size <- file.info(file_path)$size

  readChar(file_path, size, useBytes = TRUE)
}

get_header <- function(rtf_page){
  header <- rtf_page %>%
    str_extract("(?s)(?<=\\{\\\\header).*")
  header <- paste0("{\\header",header)
  header <- strsplit(header, split = "")[[1]] %>%
    get_section()
}

# count_till_section <- function(input_text){
#   if(purrr::is_empty(input_text) || input_text[1] == "{"){
#     return(0)
#   }
#   return(count_till_section(input_text[-1]) + 1)
# }
#
# get_sections <- function(input_text){
#   if(purrr::is_empty(input_text)){
#     return(c())
#   }
#   n_till_section <- count_till_section(input_text)
#   input_text <- input_text[(n_till_section+1):length(input_text)]
#   next_section <- get_section(input_text)
#   return(c(next_section,get_sections(input_text[(nchar(next_section)+1):length(input_text)])))
#
# }
#
get_section <- function(input_text, cbracket_count = 0){
  if(purrr::is_empty(input_text)){
    return("")
  }
  if(input_text[1] == "{"){
    cbracket_count <- cbracket_count + 1
  }else if(input_text[1] == "}"){
    cbracket_count <- cbracket_count - 1
  }
  if(cbracket_count == 0){
    return(input_text[1])
  }
  return(paste0(input_text[1],get_section(input_text[-1],cbracket_count),collapse = ""))
}




get_df_template <- function(rows,max_ncells){
  nchr_cols <- NULL
  ndata_cols <- NULL

  for(i in seq_along(rows)){
    if(rows[i]$header == FALSE && rows[i]$ncells == max_ncells){
      nchr_cols <- rows[i]$texts %>%
        sapply(function(x) grepl("[A-Za-z]",x))
      ndata_cols <- max_ncells - nchr_cols
      break
    }
  }

  if(!is.null(ndata_cols) && !is.null(nchr_cols)){
    # headers <- c(produce_col_names("ROWORD",nchr_cols),produce_col_names(houdini_global$defaults$rowlbls.name,nchr_cols),produce_col_names(houdini_global$defaults$cols.name,ndata_cols))
    headers <- c(produce_col_names(houdini_global$defaults$rowlbls.name,nchr_cols),produce_col_names(houdini_global$defaults$cols.name,ndata_cols))
    df <- data.frame(matrix(ncol = (1*nchr_cols + ndata_cols), nrow = 0))
    names(df) <- headers
  }
  else{
    stop("Table has no data")
  }
  df
}

produce_col_names <- function(name_type, ncols){
  names <- character(ncols)
  for(i in seq_len(ncols)){
    names[i] <- sprintf("%s%s",name_type,i)
  }
  names
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


compile_rows <- function(rtf_rows, hide_data = FALSE, produce_df = FALSE){
  rows <- rtf_rows %>%
    lapply(function(x) extract_rtf_row(x)) %>%
    unname()

  found_header <- FALSE
  max_widths <- get_max_widths(rows)
  max_ncells <- length(max_widths)

  if(produce_df){
    df <- get_df_template(rows)
    df_headers <- names(df)
  }

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
      }else if(row$ncells == max_ncells){ #add a bottom most header row
        xml_rows <- c(xml_rows,generate_xml_row(row$texts,bold = TRUE, alignment = row$alignments, part = "header", header = 1, keep_with_next = TRUE, spans = spans, hide_data = hide_data))
        if(produce_df)
          labels <-row$texts
      }else{ # add an additional layer of headers
        xml_rows <- c(xml_rows,generate_xml_row(row$texts,bold = TRUE, alignment = row$alignments, part = "header", header = 2, keep_with_next = TRUE, spans = spans, hide_data = hide_data))
      }
    }else{
      xml_rows <- c(xml_rows,generate_xml_row(row$texts, alignment = row$alignments, part = "body", keep_with_next = keep_with_next, spans = spans, hide_data = hide_data))
      if(produce_df && row$ncells == max_ncells){
        df_row <- row$texts
        df <- df %>%
          rbind(df_row)
      }
    }
    prev_ncells <- row$ncells

  }
  if(produce_df){
    names(df) <- df_headers
    haven::write_sas()
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
