#' Adds a second layer of headers to the header labels
#'
#' @param data a data frame for which you want to add a second header layer
#' @param second_labels a character vector of the types of labels of the second header layer
#' @param header_code a character vector that defines which labels belong to which column
#'
#' @return a data frame with a second layer of headers added to the labels of the header
#' @importFrom purrr is_empty
#' @keywords internal
#'
apply_second_header <- function(data,header_code){
  second_labels <- houdini_global$header_code_table

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




#' Moves a secondary layer of label headers from a marked column in a dataframe to the column labels with a delimiter
#'
#' @param data a dataframe
#'
#' @return a dataframe with second layer of labels appended
#' @importFrom dplyr select all_of
#' @keywords internal
#'
apply_trt_headers <- function(data){
  second_labels <- data %>%
    dplyr::select(dplyr::starts_with(houdini_global$defaults$trtlbls.name))

  labels <- data %>%
    get_labels()
  col_numbers <- second_labels %>%
    names() %>%
    sapply(function(x){
      gsub(houdini_global$defaults$trtlbls.name, "", x) %>%
        strsplit(split = "") %>%
        sapply(function(y) sprintf("%s%s",houdini_global$defaults$cols.name,y))
    })
  for(i in 1:ncol(col_numbers)){
    for(j in 1:nrow(col_numbers)){
      labels[col_numbers[j,i]] <- sprintf("%s%s%s", second_labels[[names(col_numbers[j,i])]][1], houdini_global$defaults$delimiter.non.regex, labels[col_numbers[j,i]])
    }
  }
  data <- data %>%
    replace_labels(labels)
  data
}
