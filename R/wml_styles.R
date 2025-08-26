create_style <- function(){
  style <- paste0(
    "<w:style w:type=\"table\" w:styleId=\"houdinitable\">",
    "<w:name w:val=\"Houdini Table\"/>",
    "<w:basedOn w:val=\"TableNormal\"/>",
    "<w:uiPriority w:val=\"99\"/>",
    "<w:qFormat/>",
    "<w:tblPr>",
    "<w:jc w:val=\"centre\"/>",
    "<w:tblBorders>",
    "<w:top w:val=\"single\" w:sz=\"24\" w:color=\"000000\"/>",
    "<w:bottom w:val=\"single\" w:sz=\"24\" w:color=\"000000\"/>",
    "<w:right w:val=\"nil\"/>",
    "<w:left w:val=\"nil\"/>",
    "<w:insideH w:val=\"nil\"/>",
    "<w:insideV w:val=\"nil\"/>",
    "</w:tblBorders>",
    "</w:tblPr>",
    "<w:tblStylePr w:type=\"wholeTable\">",
    "<w:rPr>",
    "<w:rFonts w:ascii=\"Times New Roman\" w:hAnsi=\"Times New Roman\"/>",
    "<w:sz w:val=\"20\"/>",
    "<w:szCs w:val=\"20\"/>",
    "</w:rPr>",
    "</w:tblStylePr>",
    "</w:style>"

  )
  style
}

add_style <- function(style,filename){
  style_xml <- get_styles(filename)
  style_root <- xml2::xml_root(style_xml)
  new_style <- xml2::read_xml(style)
  style_root %>%
    xml2::xml_add_child(new_style)

}

create_style_df <- function(styles){
  style <- data.frame(
    style_type = "table",
    style_id = "houdinitable",
    style_name = "houdinitable",
    base_on = "TableNormal",
    is_custom = TRUE,
    is_default = FALSE,
    keep_next = FALSE,
    line_spacing = 1,
    padding.bottom = "0",
    font.size = "20",
    color = "000000",
    font.family = "Times New Roman",
    hansi.family = "Times New Roman"
  )
  add_row(styles,style)
}

get_styles <- function(filename){
  package_dir <- tempfile()
  unpack_folder(file = filename, folder = package_dir)
  file <- file.path(package_dir,"word/styles.xml")
  styles_xml <- read_xml(file)
  styles_xml
}

write_style <- function(filename){
  package_dir <- tempfile()
  unpack_folder(file = filename, folder = package_dir)
  file <- file.path(package_dir,"word/styles.xml")
}

