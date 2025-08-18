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
#' @param bookmark a string that is the id of the bookmark
#' @param value a flextable object to insert into the document
#' @param jmp_tbl a list representing a jumptable for bookmark ids and xmml nodes
#'
#' @return a word document with the table inserted at the specified bookmark
#' @export
#'
#' @examples
add_table <- function(x, bookmark, value,jmp_tbl){

  #sets cursor to each bookmark
  x <- x %>%
    cursor_to_bookmark(jmp_tbl,bookmark)

  #adds table at that cursor point
  x <-  body_add_flextable(x = x, value = value, align = "center", pos = "on")

  #returns changed doc
  x
}




#code taken from github
cursor_bookmarks <- function(x, id) {
  #tic("section 1")
  xpath_ <- sprintf("//w:bookmarkStart[@w:name='%s']", id)
  bm_start <- xml_find_first(x$doc_obj$get(), xpath_)

  if (inherits(bm_start, "xml_missing")) {
    stop("cannot find bookmark ", shQuote(id), call. = FALSE)
  }
  #toc()
  #tic("section 2")
  bm_id <- xml_attr(bm_start, "id")

  nodes_with_text <- xml_find_all(
    x$doc_obj$get(),
    "/w:document/w:body/*|/w:ftr/*|/w:hdr/*"
  )
  #toc()
  tic("section 3")
  print(bm_id)
  print(bm_start)
  test_start <- sapply(nodes_with_text, function(node) {
    ancestors <- xml_parents(node)

    any(sapply(nodes_with_text, function(section){
      any(xml_path(nodes_with_text[i]) == xml_path(ancestors))
    }))

  })
  test_start <- sapply(nodes_with_text, function(node) {
    #tic("Section 3.1")
    expr <- sprintf("/descendant::w:bookmarkStart[@w:id='%s']", bm_id)
    #toc()
    tic("Section 3.2")
    match_node <- xml_child(node, expr)
    tic("Section 3.3")
    !inherits(match_node, "xml_missing")
  })
  if (!any(test_start)) {
    stop("bookmark ", shQuote(id), " has not been found in the document", call. = FALSE)
  }
  toc()
  tic("section 4")
  test_end <- sapply(nodes_with_text, function(node) {
    expr <- sprintf("/descendant::w:bookmarkEnd[@w:id='%s']", bm_id)
    match_node <- xml_child(node, expr)
    !inherits(match_node, "xml_missing")
  })
  toc()
  tic("section 5")
  on_same_par <- test_start == test_end
  if (!all(on_same_par)) {
    stop("bookmark ", shQuote(id), " does not end in the same paragraph (or is on the whole paragraph)", call. = FALSE)
  }
  print("HERE:")
  print(which(test_start)[1])
  x$officer_cursor$which <- which(test_start)[1]
  toc()
  x
}



