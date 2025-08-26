#This is code from https://github.com/davidgohel/officer/blob/master/R/docx_cursor.R
#taken in to test speed as this function is the main bottleneck




#creates jumptable to all bookmarks in doc
#' Creates a jumptable for all bookmarks in a word document
#'
#' @param x a word document
#'
#' @return a list that acts as a jumptable for bookmark names and XML node values
#' @export
#'
#' @examples
find_all_bookmarks <- function(x){


  doc_xml <- x$doc_obj$get() #gets the word doc in xml format

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
  bm_jmptbl
}

#' Moves the cursor in a word document to a name bookmark
#'
#' @param x a word document in which you want to move the cursor
#' @param jmp_tbl a list that behaves as a jumptable for bookmark from finf_all_bookmarks()
#' @param id a string that is the name of a bookmark
#'
#' @return a word doc with a moved cursor
#' @export
#'
#' @examples
cursor_to_bookmark <- function(x,jmp_tbl,id){
  if(id %in% names(jmp_tbl))
  {
    x$officer_cursor$which <- jmp_tbl[id]
  }
  else
    stop("Cannot find bookmark in jumptable")

  x
}




# Set up function to add table
#' Inserts a table into a word document at a bookmark
#'
#' @param x a word document that the table is to be inserted into
#' @param bookmark a string that is the id of the bookmark where the table is to be inserted
#' @param table a flextable object to insert into the document
#' @param jmp_tbl a list representing a jumptable for bookmark ids and XML nodes
#'
#' @return a word document with the table inserted at the specified bookmark
#' @export
#'
#' @examples
add_table <- function(x, bookmark, table, jmp_tbl){

  #sets cursor to each bookmark
  x <- x %>%
    cursor_to_bookmark(jmp_tbl,bookmark)

  #adds table at that cursor point
  x <-  body_add_flextable(x = x, value = table, align = "center", pos = "on")
  log_info("{bookmark} inserted", namespace = "Houdini Logs")
  #returns changed doc
  x
}


add_xml_table <- function(x, bookmark, table, jmp_tbl){

  x <- x %>%
    cursor_to_bookmark(jmp_tbl,bookmark)

  #adds table at that cursor point
  x <-  body_add_xml(x = x, table, pos = "on")
  log_info("{bookmark} inserted", namespace = "Houdini Logs")
  #returns changed doc
  x
}


#' Title
#'
#' @param x a word document that the figure is to be inserted into
#' @param bookmark a string that is the id of the bookmark where the table is to be inserted
#' @param image
#' @param jmp_tbl a list representing a jumptable for bookmark ids and XML nodes
#' @param width the width of the image in the document (defaults to 6.5in the width of a doc with standard margins)
#' @param height the width of the image in the document
#'
#' @return
#' @export
#'
#' @examples
add_figure <- function(x, bookmark, image, jmp_tbl, width = 6.5, height = 6.5){
  x <- x %>%
    cursor_to_bookmark(jmp_tbl,bookmark)


  x <- body_add_img(x = x,width = width, height = height, src = image, pos = "on")
  log_info("{bookmark} inserted", namespace = "Houdini Logs")
  x
}

compile_xml <- function(ft){
  flextable:::gen_raw_wml(ft)
}



