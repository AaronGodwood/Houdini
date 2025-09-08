

output_docx <- function(x, target = NULL){
  if(is.null(target)){
    cat("hrdocx object with",length(x),"elements\n")
    if (length(x) > 1) {
      cursor_node <- get_cursor_block(x$cursor,x$doc)
      cat("\n* Content at cursor location:\n")
      print(xml_text(cursor_node, x))
    } else {
      cat("\n* empty document\n")
    }
    return(invisible())
  }

  if (!grepl(x = target, pattern = "\\.(docx)$", ignore.case = TRUE)) {
    stop(target, " should have '.docx' extension.")
  }

  write_xml(x$doc,paste0(x$package_dir,"/word/document.xml"))

  invisible(pack_folder(folder = x$package_dir, target = target))

}

length.hrdocx <- function(x){
  xml_length(xml_child(x$doc, "w:body"))
}

read_docx <- function(path){
  if( !is.null(path) && !file.exists(path)){
    stop(paste0("Could not find file ",path))
  }

  if(is.null(path)){
    path <- system.file(package = "Houdini", "templates/template.docx")
  }

  if(!grepl("\\.(docx|dotx)$", path, ignore.case = TRUE)){
    stop("read_docx only supports docx files")
  }


  package_dir = tempfile()
  unpack_folder(file = path, folder = package_dir)

  doc <- read_xml(paste0(package_dir,"/word/document.xml"))
  cursor <- houdini_cursor(doc)

  out <- structure(list(
    package_dir = package_dir,
    doc = doc,
    cursor = cursor),
    class = "hrdocx"
  )

  out
}

docx_dim <- function(x){
  cursor <- paste0("/w:document/w:body/*[",x$cursor$which,"]")
  if (is.na(cursor)) {
    next_section <- xml_find_first(x$doc, "/w:document/w:body/w:sectPr")
  } else {
    xpath_ <- paste0(
      file.path( cursor, "following-sibling::w:sectPr"),
      "|",
      file.path( cursor, "following-sibling::w:p/w:pPr/w:sectPr"),
      "|",
      "//w:sectPr"
    )
    next_section <- xml_find_first(x$doc, xpath_)
  }

  sd <- section_dimensions(next_section)
  sd$page <- sd$page / (20*72)
  sd$margins <- sd$margins / (20*72)
  sd
}

section_dimensions <- function(node) {
  section_obj <- as_list(node)

  landscape <- FALSE
  if (
    !is.null(attr(section_obj$pgSz, "orient")) &&
    attr(section_obj$pgSz, "orient") == "landscape"
  ) {
    landscape <- TRUE
  }

  h_ref <- as.integer(attr(section_obj$pgSz, "h"))
  w_ref <- as.integer(attr(section_obj$pgSz, "w"))

  mar_t <- as.integer(attr(section_obj$pgMar, "top"))
  mar_b <- as.integer(attr(section_obj$pgMar, "bottom"))
  mar_r <- as.integer(attr(section_obj$pgMar, "right"))
  mar_l <- as.integer(attr(section_obj$pgMar, "left"))
  mar_h <- as.integer(attr(section_obj$pgMar, "header"))
  mar_f <- as.integer(attr(section_obj$pgMar, "footer"))

  list(
    page = c("width" = w_ref, "height" = h_ref),
    landscape = landscape,
    margins = c(
      top = mar_t,
      bottom = mar_b,
      left = mar_l,
      right = mar_r,
      header = mar_h,
      footer = mar_f
    )
  )
}

pack_folder <- function(folder, target){

  target <- absolute_path(target)
  dir_fi <- dirname(target)

  if( !file.exists(dir_fi) ){
    stop("directory ", shQuote(dir_fi), " does not exist.", call. = FALSE)
  } else if( file.access(dir_fi) < 0 ){
    stop("can not write to directory ", shQuote(dir_fi), call. = FALSE)
  } else if( file.exists(target) && file.access(target) < 0 ){
    stop(shQuote(target), " already exists and is not writable", call. = FALSE)
  } else if( !file.exists(target) ){
    old_warn <- getOption("warn")
    options(warn = -1)
    x <- tryCatch({cat("", file = target);TRUE}, error = function(e) FALSE, finally = unlink(target, force = TRUE) )
    options(warn = old_warn)
    if( !x )
      stop(shQuote(target), " cannot be written, please check your permissions.", call. = FALSE)
  }

  curr_wd <- getwd()
  setwd(folder)
  tryCatch(
    zip::zipr(zipfile = target, include_directories = FALSE,
              files = list.files(path = ".", all.files = FALSE), recurse = TRUE)
    , error = function(e) {
      stop("Could not write ", shQuote(target), " [", e$message, "]")
    },
    finally = {
      setwd(curr_wd)
    })

  target
}

unpack_folder <- function(file, folder){

  stopifnot(file.exists(file))

  file_type <- gsub("(.*)(\\.[a-zA-Z0-0]+)$", "\\2", file)

  # force deletion if already existing
  unlink(folder, recursive = TRUE, force = TRUE)

  if( l10n_info()$`UTF-8` ){
    zip::unzip( zipfile = file, exdir = folder )
  } else {

    wd_folder <- tempdir()
    # unable to unzip a file with accent when on windows
    newfile <- tempfile(tmpdir = wd_folder, fileext = file_type)
    file.copy(from = file, to = newfile)
    zip::unzip( zipfile = newfile, exdir = folder )
    unlink(newfile, force = TRUE)
  }

  absolute_path(folder)
}


absolute_path <- function(x){

  if (length(x) != 1L)
    stop("'x' must be a single character string")
  epath <- path.expand(x)

  if( file.exists(epath)){
    epath <- normalizePath(epath, "/", mustWork = TRUE)
  } else {
    if( !dir.exists(dirname(epath)) ){
      stop("directory of ", x, " does not exist.", call. = FALSE)
    }
    cat("", file = epath)
    epath <- normalizePath(epath, "/", mustWork = TRUE)
    unlink(epath)
  }
  epath
}



add_xml <- function(x, str, pos = c("after", "before", "on")) {
  new_xml <- as_xml_document(str)
  pos <- match.arg(pos)

  cursor_node <- get_cursor_block(x$cursor,x$doc)
  if (is.null(cursor_node)) {
    xml_add_child(
      xml_find_first(x$doc, "/w:document/w:body"),
      xml_elt,
      .where = 0
    )
    .name <- xml_name(new_xml)
    x$cursor <- cursor_append(x$cursor, .name)
  } else if (pos == "on") {
    xml_replace(cursor_node, new_xml)
    x$cursor <- cursor_replace_nodename(
      x$cursor,
      xml_name(new_xml)
    )
  } else if (pos == "after") {
    xml_add_sibling(cursor_node, new_xml, .where = pos)
    x$cursor <- cursor_add_after(x$cursor, xml_name(new_xml))
  } else {
    xml_add_sibling(cursor_node, new_xml, .where = pos)
    x$cursor <- cursor_add_before(x$cursor, xml_name(new_xml))
  }
  x
}


houdini_cursor <- function(node){
  nodes <- xml_find_all(node, "/w:document/w:body/*")
  nodes_names <- xml_name(nodes)
  nodes_names <- nodes_names[!nodes_names %in% "sectPr"]
  out <- list(
    nodes_names = nodes_names,
    which = length(nodes_names)
  )
  class(out) <- "houdini_cursor"
  out
}

cursor_append <- function(x, what) {
  x$nodes_names <- c(x$nodes_names, what)
  x$which <- x$which + 1L
  x
}

cursor_add_after <- function(x, what) {
  seq_left <- seq_along(x$which)
  set_left <- x$nodes_names[seq_left]
  set_right <- x$nodes_names[-seq_left]
  x$nodes_names <- c(set_left, what, set_right)
  x$which <- x$which + 1L
  x
}

cursor_add_before <- function(x, what) {
  seq_left <- seq_along(x$which - 1)
  set_left <- x$nodes_names[seq_left]
  set_right <- x$nodes_names[-seq_left]
  x$nodes_names <- c(set_left, what, set_right)
  x
}

cursor_replace_nodename <- function(x, what) {
  x$nodes_names[x$which] <- what
  x
}

get_cursor_block <- function(x, node) {
  if (length(x$nodes_names) < 1) {
    return(NULL)
  }
  node <- xml_find_first(node, paste0("/w:document/w:body/*[",x$which,"]"))
  if (inherits(node, "xml_missing")) {
    stop("cursor does not correspond to any node", call. = FALSE)
  }
  node
}
