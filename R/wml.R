

xml_hello <- function(){
  hello_doc <- officer::read_docx("hello.docx")
  hello_xml <- hello_doc$doc_obj$get()
}



gen_xml <- function(ht){

  header <- ht$header$dataset
  header_spans <- ht$header$spans
  footer <- ht$footer$dataset
  footer_spans <- ht$footer$spans
  body <- ht$body$dataset
  widths <- ht$widths
  alignments <- ht$alignments
  table_properties <- generate_table_properties(width = 1, layout = "autofit") # will access width and layout from ht
  table_grid <- generate_xml_grid(widths)
  table_rows <- character(nrow(body))
  header_rows <- character(nrow(header))
  footer_rows <- character(nrow(footer))
  for(i in seq_len(nrow(body))){
    table_rows[i] <- generate_xml_row(body[i,],alignment = alignments,keep_with_next = ht$keep_with_next[i])
  }
  for(i in seq_len(nrow(header))){
    header_num <- nrow(header) - i + 1
    header_rows[i] <- generate_xml_row(header[i,],alignment = alignments, header = header_num, bold = TRUE, part = "header", spans = header_spans[i,],keep_with_next = TRUE)
  }
  for(i in seq_len(nrow(footer))){
    top_footer <- FALSE
    if(i == 1){
      top_footer <- TRUE
    }

    footer_rows[i] <- generate_xml_row(footer[i,],part = "footer", spans = footer_spans[i,],top_footer = top_footer, keep_with_next = TRUE)
  }
  table_rows <- paste(table_rows, collapse = "")
  header_rows <- paste(header_rows, collapse = "")
  footer_rows <- paste(footer_rows,collapse = "")
  paste0("<w:tbl>",table_properties,table_grid,header_rows,table_rows,footer_rows,"</w:tbl>")
}

generate_xml_row <- function(row, bold = FALSE, alignment = NULL, part = "body", keep_with_next = FALSE, header = 0, spans = NULL, top_footer = FALSE){
  header_num <- header
  if(is.null(alignment)){
    alignment = rep("start", length(row))
  }
  if(is.null(spans))
  {
    spans = rep(0, length(row))
  }
  if(part == "header"){
    header <- "<w:tblHeader/>"

  }
  else
  {
    header <- ""
  }

  row_properties <- paste0("<w:trPr><w:trHeight w:val=\"360\" w:hRule=\"auto\"/><w:cantSplit/>",header,"</w:trPr>")

  cells <- character(length(row))
  current_span = 1
  for(i in seq_along(row)){
    current_span <- current_span - 1
    cells[i] <- row[i] %>%
      escape_xml() %>%
      generate_xml_cell(bold = bold, alignment = alignment[i], header = header_num, span = spans[i], in_span = current_span,keep_with_next = keep_with_next, top_footer = top_footer)#, alignment = alignment[i])
    current_span = current_span + spans[[i]]
  }

  paste0("<w:tr>",row_properties, paste(cells, collapse = ""), "</w:tr>")

}

escape_xml <- function(text){
  text %>%
    gsub("&", "&amp;", .) %>%
    gsub("<", "&lt;", .) %>%
    gsub(">", "&gt;", .)

}

generate_xml_grid <- function(widths){
  cols <- character(length(widths))
  for(i in seq_along(widths)){
    cols[i] <- paste0("<w:gridCol w:w=\"",widths[i],"\"/>")
  }
  paste0("<w:tblGrid>",paste(cols, collapse = ""),"</w:tblGrid>")
}


generate_xml_cell <- function(text, bold = FALSE, alignment = "start", width = 4000, header = 0, span = 0, in_span = 0, keep_with_next= FALSE, top_footer = FALSE){
  if(in_span > 0)
  {
    return("")
  }
  if(bold){
    bold <- "<w:b/>"
  }
  else
  {
    bold <- ""
  }
  if(keep_with_next){
    keep_with_next <- "<w:keepNext/>"
  }
  else{
    keep_with_next <- ""
  }
  if((header >= 2 && text != "") || (header == 1)){
    borders <- "<w:tcBorders><w:bottom w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/></w:tcBorders>"
    vAlign <- "<w:vAlign w:val=\"bottom\"/>"
    keep_with_next <- "<w:keepNext/>"
  }
  else
  {
    borders = ""
    vAlign <- ""
  }

  if(is.na(text) || text == "NA"){
    text <- ""
  }
  else{
    text <- process_text(text)
  }

  if(span != 0){
    merge = paste0("<w:gridSpan w:val=\"",span,"\"/>")
  }
  else{
    merge = ""
  }
  if(top_footer){
    borders <- "<w:tcBorders><w:top w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/></w:tcBorders>"
  }


  font <- "<w:rFonts w:ascii=\"Times New Roman\" w:hAnsi=\"Times New Roman\"/>"
  font_size <- "<w:sz w:val=\"20\"/>"

  cell <- paste0(
    "<w:tcPr>",vAlign,borders,merge,"</w:tcPr>",#"<w:tcW w:w=\"",width,"\" w:type=\"pct\"/>",
    "<w:p>",
    "<w:pPr><w:spacing w:before=\"0\" w:after=\"0\" w:line=\"240\"/>",keep_with_next,"<w:jc w:val=\"", alignment,"\"/></w:pPr>",
    "<w:r><w:rPr>",font,font_size, bold, "</w:rPr>",text,"</w:r>",
    "</w:p>"
  )

  paste0("<w:tc>", cell, "</w:tc>")
}

generate_table_properties <- function(width = 1, layout){
  width <- width * 5000
  properties <- paste0(
    "<w:tblLayout w:type=\"",layout,"\"/>",
    "<w:tblW w:w=\"",width,"\" w:type=\"pct\"/>",
    # "<w:tblCellMar>",
    # "<w:top w:w=\"0\" w:type=\"dxa\"/>",
    # "<w:bottom w:w=\"0\" w:type=\"dxa\"/>",
    # "<w:left w:w=\"0\" w:type=\"dxa\"/>",
    # "<w:right w:w=\"0\" w:type=\"dxa\"/>",
    # "</w:tblCellMar>",
    "<w:tblBorders>",
    "<w:top w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/>",
    "<w:bottom w:val=\"none\"/>",
    "<w:left w:val=\"none\"/>",
    "<w:right w:val=\"none\"/>",
    "<w:insideH w:val=\"none\"/>",
    "<w:insideV w:val=\"none\"/>",
    "</w:tblBorders>"
  )

  paste0("<w:tblPr>", properties, "</w:tblPr>")
}


process_text <- function(text){
  if(grepl("\\\n",text)){
    texts <- strsplit(text,"\\\n") %>%
      unlist()
    texts[1:(length(texts)-1)] <- texts[-(length(texts))] %>%
      sapply(function(x){
        paste0("<w:t xml:space=\"preserve\">", x, "</w:t><w:br/>")
      })
    texts[length(texts)] <- paste0("<w:t xml:space=\"preserve\">", texts[length(texts)], "</w:t>")
    new_text <- paste(texts,collapse = "")
  }else{
    new_text <- paste0("<w:t xml:space=\"preserve\">", text, "</w:t>")
  }

  new_text
}





