#' @importFrom xml2 xml_find_first xml_ns read_xml xml_add_sibling xml_children
add_rel <- function(x,id,image_name){
  rels_node <- xml_find_first(x$rels, "//d1:Relationships", xml_ns(x$rels))
  rel_xml <- read_xml(sprintf("<Relationship Id=\"rId%s\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/image\" Target=\"media/%s\"/>",id,image_name))
  xml_add_child(rels_node, rel_xml)
  x
}

#' @importFrom xml2 xml_find_first xml_root xml_add_child xml_attrs
add_png_extension <- function(content){
  exists <- xml_find_all(content,"//d1:Default[@Extension='png']")

  if(length(exists) < 1){
    root <- xml_root(content)
    first_node <- xml_children(root)[[2]]
    node <- "<Default Extension=\"png\" ContentType=\"image/png\"/>"
    xml_add_sibling(first_node, read_xml(node), .where = "before")
  }
  content
}

#' @importFrom xml2 xml_ns xml_attr xml_find_all
get_next_rel_id <- function(rels,ids = c()){
  new_ids <- xml_attr(xml_find_all(rels, "//d1:Relationships/*", xml_ns(rels)),"Id") %>%
    sub("rId", "",.) %>%
    as.integer()
  ids <- c(ids,new_ids[!is.na(new_ids)])
  next_num <- ifelse(length(ids) == 0, 1, max(ids)+1)
  paste0("", next_num)

}

add_img <- function(x, src, width, height, pos = "after"){
  unit <- "in"
  ids <- xml_attr(xml_find_all(x$doc, "//wp:docPr"), "id") %>%
    as.integer()

  #ids <- ids[ids <1000]
  file_type <- gsub("(.*)(\\.[a-zA-Z0-0]+)$", "\\2", src)
  file_title <- gsub(file_type, "", src, fixed = TRUE)

  srcc <- src
  attr(src, "dims") <- list(width = width, height = height)
  attr(src, "alt") <- ""

  id <- get_next_rel_id(x$rels,ids)

  image_name <- paste0(basename(file_title),id,file_type)
  x <- add_rel(x,id,image_name)
  x$content <- add_png_extension(x$content)
  xml_img <- gen_img_wml(src,id)
  media_dir <- file.path(x$package_dir,"word","media")
  if(!dir.exists(media_dir)){
    dir.create(media_dir, recursive = TRUE)
  }

  file.copy(srcc, file.path(media_dir, image_name))

  add_xml(x = x, str = xml_img, pos = pos)
  x

}


gen_img_wml <- function(src, id){

  dims <- attr(src, "dims")
  width <- dims$width
  height <- dims$height

  blipfill <- paste0(
    "<pic:blipFill>",
    sprintf("<a:blip r:embed=\"rId%s\"/>", id),
    "<a:stretch><a:fillRect/></a:stretch>",
    "</pic:blipFill>")

  xml_img <- paste0(
    "<w:r>",
    "<w:rPr><w:noProof/></w:rPr><w:drawing xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><wp:inline distT=\"0\" distB=\"0\" distL=\"0\" distR=\"0\">",
    sprintf(
      "<wp:extent cx=\"%.0f\" cy=\"%.0f\"/>",
      width * 12700 * 72,
      height * 12700 * 72
    ),
    sprintf("<wp:docPr id=\"%s\" name=\"Picture %s\" descr=\"",id,id),
    attr(src, "alt"),
    "\"/>",
    "<wp:cNvGraphicFramePr><a:graphicFrameLocks xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" noChangeAspect=\"1\"/></wp:cNvGraphicFramePr>",
    "<a:graphic xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/picture\"><pic:pic xmlns:pic=\"http://schemas.openxmlformats.org/drawingml/2006/picture\">",
    "<pic:nvPicPr>",
    sprintf("<pic:cNvPr id=\"0\" name=\"Picture %s\"/>",id),
    "<pic:cNvPicPr><a:picLocks noChangeAspect=\"1\" noChangeArrowheads=\"1\"/>",
    "</pic:cNvPicPr></pic:nvPicPr>",
    blipfill,
    "<pic:spPr bwMode=\"auto\"><a:xfrm><a:off x=\"0\" y=\"0\"/>",
    sprintf(
      "<a:ext cx=\"%.0f\" cy=\"%.0f\"/></a:xfrm><a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:noFill/></pic:spPr>",
      width * 12700,
      height * 12700
    ),
    "</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>"
  )

  xml_img <- paste0("<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">",
                    xml_img,"</w:p>")
}
