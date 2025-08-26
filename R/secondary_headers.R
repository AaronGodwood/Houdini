#' Adds a second layer of headers to the header labels
#'
#' @param data a data frame for which you want to add a second header layer
#' @param second_labels a character vector of the types of labels of the second header layer
#' @param header_code a character vector that defines which labels belong to which column
#'
#' @return a data frame with a second layer of headers added to the labels of the header
#' @export
#'
#' @examples
apply_second_header <- function(data,second_labels,header_code){


  #only runs if there are secondary headers
  if(!purrr::is_empty(header_code)){

    #gets label attribute for each column
    labels <- data %>%
      get_labels()

    #appends second header to beginning of header with
    for(i in 1:length(labels)){
      if(header_code[i] != "0"){
        labels[i] <- sprintf("%s^*^%s",second_labels[[header_code[i]]],labels[i])
      }
    }

    #changes labels
    data <- data %>%
      replace_labels(labels, add = FALSE)
  }
  #returns changed or unchanged data
  data
}

#brings secondary headers marked by the ^*^ delimiter up to a second and higher row of headers # may be defunct
#' Turn a single layer of headers split by delimiters to a multi-layered header
#'
#' @param ft a flextable object for which the headers need lifting
#' @param second_headers a list of character vectors that represent the 2nd and upwards layers of headers
#'
#' @return a flextable object with lifted headers
#' @export
#'
#' @examples
lift_headers <- function(ht,second_headers){


  #if there are no second layer of headers return the flextable as is
  if(purrr::is_empty(second_headers)){
    ft
  }

  #standardises second_headers so that ell elements are of length 2 and bind it into a data frame
  second_headers <- second_headers %>%
    sapply(function(x){
      if(length(x) != max(lengths(second_headers))){
        x <- x %>%
          append(rep("",(max(lengths(second_headers))-length(x))))
      }
      x
    }) %>%
    rbind()


  #adds each new layer of headers to the table
  for( k in dim(second_headers)[1]:1){
    border_cols <- second_headers[k,] %>%
      sapply(function(x){

        if(x == ""){
          FALSE
        }
        else
          TRUE
      }) %>%
      which()


    ft <- ft %>%
      add_header_row(values = second_headers[k,], top = TRUE ) %>%
      hline(j=border_cols, i = (dim(second_headers)[1] - k + 1),border = header_border,part = "header")

    ft
  }


  #futher formats the table so it all looks normal
  ft <- ft %>%
    hline(i = (dim(second_headers)[1]+1),part = "header", border = header_border) %>%
    hline_top(part = "header", border = header_border) %>%
    hline_bottom(part = "body" , border = header_border) %>%
    merge_h(part = "header")


  #returns the table
  ft
}
