read_raw_rtf <- function(file_path){
  size <- file.info(file_path)$size

  readChar(file_path, size, useBytes = TRUE)
}


build_table <- function(raw_rtf,filters, hide_data = FALSE){
  rtf_pages <- get_pages(raw_rtf,filters)
  table_properties <- generate_table_properties(width = 1, layout = "fixed") # will access width and layout from ht
  #filter
  xml_rows <- rtf_pages %>%
    compile_rows(filters,hide_data)
  paste0("<w:tbl>",table_properties,xml_rows,"</w:tbl>")
}


compile_footer <- function(footer,max_ncells){
  footnotes <- footer$footnotes
  if(length(footnotes) == 0){
    footnotes <- c(" ")
  }
  spans <- c(max_ncells,rep(0,(max_ncells-1)))
  xml_footers <- character(length(footnotes))
  for(i in seq_along(footnotes)){
    top_footer <- FALSE
    if(i == 1){
      top_footer <- TRUE
    }

    xml_footers[i] <- generate_xml_row(footnotes[i],part = "footer", spans = spans ,top_footer = top_footer, keep_with_next = TRUE)
  }
  paste0(xml_footers,collapse = "")
}

#' Converts rows of a table in rtf markup to a table in WordML
#'
#' @param rtf_rows rows of a table in rtf markup
#' @param filters any filters to filter out certain rows
#' @param hide_data an option to hide any data in output by replacing it with XX, defaults to FALSE
#'
#' @return a WordML table
#' @importFrom logger log_warn
#' @importFrom purrr is_empty
#' @keywords internal
#'
compile_rows <- function(rtf_rows, filters, hide_data = FALSE){#, produce_df = FALSE){
  rows <- rtf_rows$rtf %>%
    unlist() %>%
    lapply(function(x) extract_rtf_row(x)) %>%
    unname()

  found_header <- FALSE
  max_widths <- get_max_widths(rows)
  max_ncells <- length(max_widths)


  row_timelines <- rows %>%
    sapply(function(x){
      return(x$texts[1])
    })
  timelines <- filters$timelines %>%
    sapply(function(x){
      if(x %in% row_timelines)
      {
        x
      }
      else{
        logger::log_warn("Timeline filter: {x} not found - timeline filter not applied", namespace = "Houdini Log")
        c()
      }
    })
  in_filter <- FALSE
  if(purrr::is_empty(timelines)){
    timelines <- unique(row_timelines)
  }

  # if(produce_df){
  #   df <- get_df_template(rows)
  #   df_headers <- names(df)
  # }

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
        # if(produce_df)
        #   labels <-row$texts
      }else{ # add an additional layer of headers
        xml_rows <- c(xml_rows,generate_xml_row(row$texts,bold = TRUE, alignment = row$alignments, part = "header", header = 2, keep_with_next = TRUE, spans = spans, hide_data = hide_data))
      }
    }else{
      if(row_timelines[i] %in% timelines)
      {
        in_filter <- TRUE
      }
      else if(grepl("[A-Za-z]",row_timelines[i])){
        in_filter <- FALSE
      }
      if(in_filter){
        xml_rows <- c(xml_rows,generate_xml_row(row$texts, alignment = row$alignments, part = "body", keep_with_next = FALSE, spans = spans, hide_data = hide_data))
      }

      # if(produce_df && row$ncells == max_ncells){
      #   df_row <- row$texts
      #   df <- df %>%
      #     rbind(df_row)
      # }
    }
    prev_ncells <- row$ncells

  }
  # if(produce_df){
  #   names(df) <- df_headers
  #   haven::write_sas()
  # }
  xml_rows <- paste0(xml_rows, collapse = "")
  xml_footers <- compile_footer(rtf_rows$footer,max_ncells)
  paste0(xml_grid,xml_rows,xml_footers)
}

#' Extracts data from a row of an rtf table
#'
#' @param row a row from an rtf table
#'
#' @return key information about cells in an rtf row
#' @importFrom stringr str_match
#' @keywords internal
#'
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



#' Extracts the text from an rtf cell
#'
#' @param cell_prop an rtf line containing the text
#'
#' @return the text from an rtf cell
#' @importFrom stringr str_match
#' @keywords internal
#'
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
