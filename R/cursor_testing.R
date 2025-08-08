#This is code from https://github.com/davidgohel/officer/blob/master/R/docx_cursor.R
#taken in to test speed as this function is the main bottleneck
library(xml2)

x <- input_doc
#my attempt to rewrite the code
#creates jumptable to all bookmarks in doc
find_all_bookmarks <- function(x){
  tic("Jumptable Creation")

  doc_xml <- x$doc_obj$get()

  bm_starts <- xml_find_all(doc_xml,"//w:bookmarkStart[not(starts-with(@w:name, '_'))]")

  nodes_with_text <- xml_find_all(
    doc_xml,
    "/w:document/w:body/*|/w:ftr/*|/w:hdr/*")

  matches <- sapply(bm_starts, function(node){
    ancestors <- xml_parents(node)
    for(i in 1:length(nodes_with_text)){
      if(any(xml_path(nodes_with_text[i]) == xml_path(ancestors))){
        index <- i
        break
      }
    }
    index
  })

  bm_jmptbl <- set_names(matches, sapply(bm_starts,function(node) xml_attr(node, "name")))



  toc()
  bm_jmptbl
}

cursor_to_bookmark <- function(x,jmp_tbl,id){
  if(id %in% names(jmp_tbl))
  {
    print("were good")
  }

  x$officer_cursor$which <- jmp_tbl[id]
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


doc <- input_doc
add_table(doc,std_bookmarks[1],sas_data)
print("done")
