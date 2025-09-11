
gen_xml_landscape <- function(ht, hide_data = FALSE){
  header <- ht$header$dataset %>%
    t() %>%
    data.frame()
  header_spans <- ht$header$spans  %>%
    t() %>%
    data.frame()
  n_headers <- ncol(header)
  footer <- ht$footer$dataset
  footer_spans <- ht$footer$spans
  body <- ht$body$dataset  %>%
    t() %>%
    data.frame()
  widths <- ht$widths
  alignments <- ht$alignments
  table_properties <- generate_table_properties(width = 1, layout = "autofit") # will access width and layout from ht
  table_grid <- generate_xml_grid(widths)
  table <- cbind(header,body)
  table_rows <- character(nrow(table))
  for(i in seq_len(nrow(table))){
    table_rows[i] <- generate_xml_row(table[i,],landscape = TRUE, n_headers = n_headers)
  }
  table_rows <- paste0(table_rows, collapse = "")
  paste0("<w:tbl>",table_properties,table_grid,table_rows,"</w:tbl>")
}




#' Compiles a WordXML output from a houdini table object
#'
#' @param ht a houdinitable object to be compiled to Word XMl
#'
#' @return a string containing a compiled word table in WordXML format
#' @keywords internal
#'
gen_xml <- function(ht, hide_data = FALSE){

  header <- ht$header$dataset
  header_spans <- ht$header$spans
  footer <- ht$footer$dataset
  footer_spans <- ht$footer$spans
  body <- ht$body$dataset
  widths <- ht$widths
  alignments <- ht$alignments
  table_properties <- generate_table_properties(width = 1, layout = "fixed") # will access width and layout from ht
  table_grid <- generate_xml_grid(widths)
  table_rows <- character(nrow(body))
  header_rows <- character(nrow(header))
  footer_rows <- character(nrow(footer))
  for(i in seq_len(nrow(body))){
    if(i == nrow(body)){
      bottom_row <- TRUE
    }else{
      bottom_row <- FALSE
    }
    table_rows[i] <- generate_xml_row(body[i,],alignment = alignments,keep_with_next = FALSE ,bottom_row = bottom_row,hide_data = hide_data) #keep with next has been changed to false for all body elements o request of MW
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

    footer_rows[i] <- generate_xml_row(footer[i,],part = "footer", spans = footer_spans[i,], keep_with_next = TRUE)
  }
  table_rows <- paste0(table_rows, collapse = "")
  header_rows <- paste0(header_rows, collapse = "")
  footer_rows <- paste0(footer_rows,collapse = "")
  paste0("<w:tbl>",table_properties,table_grid,header_rows,table_rows,footer_rows,"</w:tbl>")
}



#' Generates a WordXML output for a row element part of a table
#'
#'
#' Here <tblHeader/> tag is added which means that these rows repeat over a page
#' Here the height is set to auto so the height of the cell fits to its contents
#'
#' @param row a row of a data frame to be compiled to a wordXML row
#' @param bold a Boolean that says weather or not the row should be bold
#' @param alignment a string that represents the text alignment of the cell in the row e.g. "start", "end" or "center"
#' @param part a string that says what part of the table the row is in e.g. "header", "footer" or "body"
#' @param keep_with_next a Boolean that says if the row should be kept on the same page as the next row in a word doc
#' @param header a number that says if the row is a header row and if it is weather it is the bottom most or not
#' @param spans an integer vector representing any spans (merged cells) in the row
#' @param top_footer a Boolean that says if the row is the topmost footer row
#' @param landscape if the table needs to be generated in landscape
#' @param n_headers tells the function how many headers there are, for use with a landscape table
#'
#' @return a WordXMl output of a row element
#' @keywords internal
#'
generate_xml_row <- function(row, bold = FALSE, alignment = NULL, part = "body", keep_with_next = FALSE, header = 0, spans = NULL, bottom_row = FALSE, hide_data = FALSE, landscape = FALSE, n_headers = 0){
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
    if(i < n_headers && landscape == TRUE){
      header_num <- 2
      bold <- TRUE
    }else if(i == n_headers && landscape == TRUE){
      header_num <- 1
      bold <- TRUE
    }else if(landscape == TRUE){
      header_num <- 0
      bold <- FALSE
    }
    current_span <- current_span - 1
    cells[i] <- row[i] %>%
      escape_xml() %>%
      generate_xml_cell(bold = bold, alignment = alignment[i], header = header_num, span = spans[i], in_span = current_span,keep_with_next = keep_with_next, bottom_row = bottom_row, hide_data = hide_data, landscape = landscape)#, alignment = alignment[i])
    current_span = current_span + spans[[i]]
  }

  paste0("<w:tr>",row_properties, paste0(cells, collapse = ""), "</w:tr>")

}

#' Replaces certain special characters with their XML counterparts
#'
#' @param text text to have special characters replaced
#'
#' @return text with special characters replaced
#' @keywords internal
#'
escape_xml <- function(text){
  text %>%
    gsub("&", "&amp;", .) %>%
    gsub("<", "&lt;", .) %>%
    gsub(">", "&gt;", .)

}

#' Creates the grid part of a wordXML table
#'
#' @param widths a vector that describes the widths of each column
#'
#' @return a table grid xml string
#' @keywords internal
#'
generate_xml_grid <- function(widths){
  cols <- character(length(widths))
  for(i in seq_along(widths)){
    cols[i] <- paste0("<w:gridCol w:w=\"",widths[i],"\"/>")
  }
  paste0("<w:tblGrid>",paste0(cols, collapse = ""),"</w:tblGrid>")
}


#' Generates a WordXML output for a cell of a table
#'
#'This contains the formatting for each cell:
#'-Font size is declared here (1 unit here is 1/2 a unit in word font size e.g. 20 here is 10 in word)
#'-Font is declared here (ascii is standards and then hAnsi is for special characters ect)
#'-Cell alignment (start for left aligned, center for center and end for right)
#'-Sets spacing before and after text to 0 so no random space around text
#'
#'
#'
#' @param text a string representing the contents of the cell
#' @param bold a Boolean that says weather the text should be bold (will be bold if the cell is part of  header row)
#' @param alignment a string representing the text alignment of the cell (should be either start, end or center)
#' @param width a float that represents the width of the cell
#' @param header an integer that says what part of the header the cell is in e.g. (0 - not a header, 1 - bottom row of headers, 2+ - not bottom row of headers)
#' - If the header is a bottom row or not a bottom row and has content it gets aligned to the bottom of the cell and gets a border below it
#' @param span an integer that represents if this cell is the start of a span (merged section) of cells so it says to take up the space of n columns
#' @param in_span an number that if greater than 0 says its in the span of another cell so not to generate
#' @param keep_with_next a Boolean that says that the cells in this row should be kept on the same page as the cells of the next row
#' @param top_footer a Boolean that says if it is the topmost row of footers which if true will add a border above it
#'
#' @return a string representing a WordXMl table cell
#' @keywords internal
#'
generate_xml_cell <- function(text, bold = FALSE, alignment = "start", width = 4000, header = 0, span = 0, in_span = 0, keep_with_next= FALSE, bottom_row = FALSE, hide_data = FALSE,landscape = FALSE){
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
    if(landscape){
      borders <- "<w:tcBorders><w:right w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/></w:tcBorders>"
      vAlign <- "<w:vAlign w:val=\"center\"/>"
    }else{
      borders <- "<w:tcBorders><w:bottom w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/></w:tcBorders>"
      vAlign <- "<w:vAlign w:val=\"bottom\"/>"
      keep_with_next <- "<w:keepNext/>"
    }
  }
  else
  {
    borders = ""
    vAlign <- ""
  }
  if(landscape){
    text_dir <- "<w:textDirection w:val=\"btLr\"/>"
  }else{
    text_dir <- ""
  }

  if(is.na(text) || text == "NA"){
    text <- ""
  }
  else if(hide_data && (!grepl("[A-Za-z]", text) && text != "")){
    text <- "<w:t xml:space=\"preserve\">XX</w:t>"
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
  if(bottom_row){
    borders <- "<w:tcBorders><w:bottom w:val=\"single\" w:sz=\"12\" w:color=\"000000\"/></w:tcBorders>"
  }


  font <- "<w:rFonts w:ascii=\"Times New Roman\" w:hAnsi=\"Times New Roman\"/>"
  font_size <- "<w:sz w:val=\"20\"/>"

  cell <- paste0(
    "<w:tcPr>",vAlign,borders,merge,"</w:tcPr>",#"<w:tcW w:w=\"",width,"\" w:type=\"pct\"/>",
    "<w:p>",
    "<w:pPr><w:spacing w:before=\"0\" w:after=\"0\" w:line=\"240\"/>",keep_with_next,text_dir,"<w:jc w:val=\"", alignment,"\"/></w:pPr>",
    "<w:r><w:rPr>",font,font_size, bold, "</w:rPr>",text,"</w:r>",
    "</w:p>"
  )

  paste0("<w:tc>", cell, "</w:tc>")
}

#' Generates the table properties part of a WordXML table
#'
#'Here a Top border is declared but no other borders - others come from cells in header or footer rows
#'Here the width of the whole table is declared with 5000pct being 100%
#'
#'
#' @param width the width the whole table should be as a fraction of the entire document
#' @param layout
#'
#' @return an XML output of the table properties part of a wordXMLtable
#' @keywords internal
#'
generate_table_properties <- function(width = 1, layout){
  width <- width * 5000
  properties <- paste0(
    "<w:tblLayout w:type=\"",layout,"\"/>",
    "<w:tblW w:w=\"",width,"\" w:type=\"pct\"/>",
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


#' Adds in line breaks to text sections that contain \\n
#'
#' \\n means nothing in XML so they lines have to be put into individual text tags with linebreak tags in between
#'xml:space = preserve means that spaces at the beginning of cell stay otherwise they disappear
#'
#' @param text the section of text that line breaks need to be added to
#'
#' @return an XML output of a series of text tags with line breaks if they occur
#' @keywords internal
#'
process_text <- function(text){
  if(grepl("\\\n",text)){
    texts <- strsplit(text,"\\\n") %>%
      unlist()
    texts[1:(length(texts)-1)] <- texts[-(length(texts))] %>%
      sapply(function(x){
        paste0("<w:t xml:space=\"preserve\">", x, "</w:t><w:br/>")
      })
    texts[length(texts)] <- paste0("<w:t xml:space=\"preserve\">", texts[length(texts)], "</w:t>")
    new_text <- paste0(texts,collapse = "")
  }else{
    new_text <- paste0("<w:t xml:space=\"preserve\">", text, "</w:t>")
  }

  new_text
}





