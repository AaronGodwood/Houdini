compile_wml <- function(ht, ...){
  align <- ht$properties$align

  align <- align %>%
    match.arg(c("centre", "left", "right"), several.ok = FALSE)
  align <- c("center" = "centre", "left" = "start", "right" = "end")[align]

  dims <- dim(ht)
  widths <- dims$widths

  out <- paste0()

  if(ht$properties$layout %in% "autofit"){
    pt <- prop_table(
      style = NULL,
      layout = table_layout(type = "autofit"),
      align = align,
      width = table_width(width = ht$properties$width, unit = "pct"),
      colwidths = table_colwidths(double(0L)),
      word_title = ht$properties$word_title,
      word_description = ht$properties$word_description
    )
  }
  else
  {
    pt <- prop_table(
      style = NULL,
      layout = table_layout(type = "fixed"),
      align = align,
      width = table_width(width = sum(widths, na.rm = TRUE), unit = "in"),
      colwidths = table_colwidths(widths),
      word_title = ht$properties$word_title,
      word_description = ht$properties$word_description
    )
  }

  properties_str <- to_wml(pt)

  out <- paste0(out, properties_str)

  tab_str <- wml_rows
}

xml_hello <- function(){
  hello_doc <- officer::read_docx("hello.docx")
  hello_xml <- hello_doc$doc_obj$get()
}

xml_test <- function(data){
  table_properties <- generate_table_properties(1, "autofit")
  nrows <- nrow(data)
  table_rows <- character(nrows)
  for(i in 1:nrows){
    table_rows[i] <- generate_xml_row(data[i,])
  }
  table_rows <- paste(table_rows, collapse = "")
  group <- paste0("<w:tr>",
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
                  table_rows,
                  "</w:tbl>",
                  "</w:tc>",
                  "</w:tr>")
  paste0("<w:tbl>",table_properties,group,"</w:tbl>")
}


gen_xml <- function(ht){
  groups <- split(ht$body$dataset, ht$body$dataset$PAGE)
  table_properties <- generate_table_properties(width = 1, layout = "autofit") # will access width and layout from ht
  table_rows <- character(length(groups))
  for(i in seq_along(groups)){
    table_rows[i] <- create_group(groups[[i]])
  }
  table_rows <- paste(table_rows, collapse = "")
  table <- sprintf("<w:tbl>%s%s</w:tbl>",table_properties,table_rows)
}

generate_xml_row <- function(row, bold = FALSE, alignment = NULL,header = FALSE){

  if(is.null(alignment)){
    alignment = rep("start", length(row))
  }
  if(header){
    header <- "<w:tblHeader/>"
  }
  else
  {
    header <- ""
  }
  row_properties <- paste0("<w:trPr><w:cantSplit/>",header,"</w:trPr>")

  cells <- character(length(row))
  for(i in seq_along(row)){
    row[i] %>%
      escape_xml()
    cells[i] <- row[i] %>%
      escape_xml() %>%
      generate_xml_cell(bold = bold)#, alignment = alignment[i])
  }

  paste0("<w:tr>",row_properties, paste(cells, collapse = ""), "</w:tr>")

}

escape_xml <- function(text){
  text %>%
    gsub("&", "&amp;", .) %>%
    gsub("<", "&lt;", .) %>%
    gsub(">", "&gt;", .)

}


generate_xml_cell <- function(text, bold = FALSE, alignment = "start", width = 4000, header = 0){
  if(bold){
    bold <- "<w:rPr><w:b/></w:rPr>"
  }
  else
  {
    bold <- ""
  }
  if((header == 2 && text != "") || (header == 1)){
    borders <- "<w:tcBorders><w:bottom w:val=\"single\" w:sz=\"4\" w:color=\"000000\"/></w:tcBorders>"
  }
  else
  {
    borders = ""
  }


  cell <- paste0(
    "<w:tcPr>",borders,"<w:tcW w:w=\"",width,"\" w:type=\"pct\"/></w:tcPr>",
    "<w:p>",
    "<w:pPr><w:jc w:val=\"", alignment,"\"/></w:pPr>",
    "<w:r>", bold, "<w:t>", text, "</w:t></w:r>",
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
    "<w:top w:val=\"single\" w:sz=\"24\" w:color=\"000000\"/>",
    "<w:bottom w:val=\"single\" w:sz=\"24\" w:color=\"000000\"/>",
    "<w:left w:val=\"none\"/>",
    "<w:right w:val=\"none\"/>",
    "<w:insideH w:val=\"none\"/>",
    "<w:insideV w:val=\"none\"/>",
    "</w:tblBorders>"
  )

  paste0("<w:tblPr>", properties, "</w:tblPr>")
}

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



