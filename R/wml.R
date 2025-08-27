

xml_hello <- function(){
  hello_doc <- officer::read_docx("M1095_HS_301_ClinicalStudyReport_Shell_V2_Draft2_Review_23June_responses_With bookmarks.docx")
  hello_xml <- hello_doc$doc_obj$get()
}



gen_xml <- function(ht){
  header <- ht$header$dataset
  header_spans <- ht$header$spans
  footer <- ht$footer$dataset
  body <- ht$body$dataset
  widths <- ht$widths
  alignments <- ht$alignments
  table_properties <- generate_table_properties(width = 1, layout = "autofit") # will access width and layout from ht
  table_grid <- generate_xml_grid(widths)
  table_rows <- character(nrow(body))
  header_rows <- character(nrow(header))
  for(i in 1:nrow(body)){
    table_rows[i] <- generate_xml_row(body[i,],alignment = alignments)
  }
  for(i in 1:nrow(header)){
    header_num <- nrow(header) - i + 1
    header_rows[i] <- generate_xml_row(header[i,],alignment = alignments, header = header_num, bold = TRUE, part = "header", spans = header_spans[i,])
  }
  table_rows <- paste(table_rows, collapse = "")
  header_rows <- paste(header_rows, collapse = "")
  paste0("<w:tbl>",table_properties,table_grid,header_rows,table_rows,"</w:tbl>")
}

generate_xml_row <- function(row, bold = FALSE, alignment = NULL, part = "body", keep_with_next = FALSE, header = 0, spans = NULL){
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

  if(keep_with_next){
    keep_with_next <- "<keepNext/>"
  }
  else
  {
    keep_with_next = ""
  }

  row_properties <- paste0("<w:trPr><w:cantSplit/>",keep_with_next,header,"</w:trPr>")

  cells <- character(length(row))
  current_span = 1
  for(i in seq_along(row)){
    current_span <- current_span - 1
    cells[i] <- row[i] %>%
      escape_xml() %>%
      generate_xml_cell(bold = bold, alignment = alignment[i], header = header_num, span = spans[i], in_span = current_span)#, alignment = alignment[i])
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


generate_xml_cell <- function(text, bold = FALSE, alignment = "start", width = 4000, header = 0, span = 0, in_span = 0){
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

  if((header >= 2 && text != "") || (header == 1)){
    borders <- "<w:tcBorders><w:bottom w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/></w:tcBorders>"
    vAlign <- "<w:vAlign w:val=\"bottom\"/>"
  }
  else
  {
    borders = ""
    vAlign <- ""
  }

  if(is.na(text) || text == "NA"){
    text <- ""
  }

  if(span != 0){
    merge = paste0("<w:gridSpan w:val=\"",span,"\"/>")
  }
  else{
    merge = ""
  }

  font <- "<w:rFonts w:ascii=\"Times New Roman\" w:hAnsi=\"Times New Roman\"/>"
  font_size <- "<w:sz w:val=\"20\"/>"

  cell <- paste0(
    "<w:tcPr>",vAlign,borders,merge,"</w:tcPr>",#"<w:tcW w:w=\"",width,"\" w:type=\"pct\"/>",
    "<w:p>",
    "<w:pPr><w:jc w:val=\"", alignment,"\"/></w:pPr>",
    "<w:r><w:rPr>",font,font_size, bold, "</w:rPr><w:t>", text, "</w:t></w:r>",
    "</w:p>"
  )

  paste0("<w:tc>", cell, "</w:tc>")
}

generate_table_properties <- function(width = 1, layout){
  width <- width * 5000
  properties <- paste0(
    "<w:tblLayout w:type=\"",layout,"\"/>",
    "<w:tblW w:w=\"",width,"\" w:type=\"pct\"/>",
    "<w:tblBorders>",
    "<w:top w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/>",
    "<w:bottom w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/>",
    "<w:left w:val=\"none\"/>",
    "<w:right w:val=\"none\"/>",
    "<w:insideH w:val=\"none\"/>",
    "<w:insideV w:val=\"none\"/>",
    "</w:tblBorders>"
  )

  paste0("<w:tblPr>", properties, "</w:tblPr>")
}

#may be massively redundant
create_group <- function(rows, bold = FALSE, alignment = NULL, header = FALSE){
  nrows <- nrow(rows)
  if(header)
  {
    headers <- rep(TRUE, nrows)
    header <- "<w:tblHeader/>"
  }
  else
  {
    headers <- rep(FALSE, nrows)
    header <- ""
  }
  xml_rows <- character(nrows)
  for(i in seq_along(nrows)){
    xml_rows[i] <- generate_xml_row(rows[1,],bold = bold, alignment = alignment)
  }
  xml_rows <- paste(xml_rows, collapse = "")

  paste0(
    "<w:tr>",
    "<w:trPr>",header,"<w:cantSplit/></w:trPr>",
    "<w:tc>",
    "<w:tcPr><w:tcW w:w=\"5000\" w:type=\"pct\"/></w:tcPr>",
    "<w:tbl>",
    "<w:tblPr>",
    "<w:tblLayout w:type=\"fixed\"/>",
    "<w:tblW w:w=\"5000\" w:type=\"pct\"/>",
    "<w:tblBorders>",
    "<w:top w:val=\"single\" w:sz=\"4\" w:color=\"000000\"/>",
    "<w:top w:val=\"single\" w:sz=\"4\" w:color=\"000000\"/>",
    "<w:left w:val=\"none\"/>",
    "<w:right w:val=\"none\"/>",
    "<w:insideH w:val=\"none\"/>",
    "<w:insideV w:val=\"none\"/>",
    "</w:tblBorders>",
    "</w:tblPr>",
    xml_rows,
    "</w:tbl>",
    "</w:tc>",
    "</w:tr>"
  )
}



