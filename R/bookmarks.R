




#' Creates a jumptable for all bookmarks in a word document
#'
#' @param x a word document
#'
#' @return a list that acts as a jumptable for bookmark names and XML node values
#' @export
#'
#' @examples
find_all_bookmarks <- function(x){


  doc_xml <- x$doc #gets the word doc in xml format

  bm_starts <- xml2::xml_find_all(doc_xml,"//w:bookmarkStart[not(starts-with(@w:name, '_'))]") #attaints all boomarks that start without _ (unhidden ones)

  nodes_with_text <- xml2::xml_find_all(
    doc_xml,
    "/w:document/w:body/*|/w:ftr/*|/w:hdr/*") #gets all sections within the document

  #gets xml locations of all sections that conatin bookmarks
  matches <- sapply(bm_starts, function(node){
    ancestors <- xml2::xml_parents(node) #gets ancestors of each bookmark node
    for(i in 1:length(nodes_with_text)){
      if(any(xml2::xml_path(nodes_with_text[i]) == xml2::xml_path(ancestors))){ #checks if a section is an ancestor of the bookmark node
        index <- i #returns the section number
        break
      }
    }
    index #returns section number
  })

  #names section numbers appropriately with bookmark names
  bm_jmptbl <- set_names(matches, sapply(bm_starts,function(node) xml2::xml_attr(node, "name")))

  #returns 'jumptable' of bookmark names and their xml locations
  houdini_global$bookmark_jmptbl <- bm_jmptbl
}

#' Moves the cursor in a word document to a named bookmark
#'
#' @param x a word document in which you want to move the cursor
#' @param id a string that is the name of a bookmark
#'
#' @return a word doc with a moved cursor
#' @export
#'
#' @examples
cursor_to_bookmark <- function(x,id){
  if(purrr::is_empty(houdini_global$bookmark_jmptbl)){
    stop("Bookmark jumptable is empty, run find_all_bookmarks()")
  }
  jmp_tbl <- houdini_global$bookmark_jmptbl
  if(id %in% names(jmp_tbl))
  {
    x$cursor$which <- jmp_tbl[id]
  }
  else
    stop("Cannot find bookmark in jumptable - Is it named right?")

  x
}



#' Adds an xml table to an rdocx object at a specified bookmark
#'
#' @param x an rdocx object for the table to be added to
#' @param bookmark the bookmark where the table is to be inserted
#' @param table an xml table to be inserted into the rdocx object
#'
#' @return an rdocx object with the table appended
#' @keywords internal
#'
#' @examples
add_xml_table <- function(x, bookmark, table){

  x <- x %>%
    cursor_to_bookmark(bookmark)

  #adds table at that cursor point
  x <-add_xml(x = x, table, pos = "next")

  #returns changed doc
  x
}


#' Adds a houdinitable object to an rdocx object at specified bookmark
#'
#' @param x an rdocx object for the table to be inserted into
#' @param bookmark the bookmark in the word document that the table is to be inserted at
#' @param ht the houdinitable object to be inserted
#'
#' @return an rdocx object with the table added
#' @export
#'
#' @examples
add_houdinitable <- function(x, bookmark, ht, hide_data = FALSE){
  #generates xml version of table
  xml_table <- gen_xml(ht,hide_data)
  #adds table to doc
  add_xml_table(x,bookmark,xml_table)
}

#' Adds a figure to a word document
#'
#' @param x a word document that the figure is to be inserted into
#' @param bookmark a string that is the id of the bookmark where the table is to be inserted
#' @param image the file location of the image to be added
#' @param jmp_tbl a list representing a jumptable for bookmark ids and XML nodes
#' @param width the width of the image in the document (defaults to 6.5in the width of a doc with standard margins)
#' @param height the width of the image in the document
#'
#' @return a document with a figure added
#' @export
#'
#' @examples
add_figure <- function(x, bookmark, image, width = 6.5, height = 6.5){
  x <- x %>%
    cursor_to_bookmark(bookmark)


  x <- body_add_img(x = x,width = width, height = height, src = image, pos = "on")
  log_info("{bookmark} inserted", namespace = "Houdini Logs")
  x
}

compile_xml <- function(ft){
  flextable:::gen_raw_wml(ft)
}



