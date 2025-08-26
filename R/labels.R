
#replaces old labels with new ones
#' Replace dataframe labels
#'
#' @param data a data frame who's labels you want to replace
#' @param new_labels a charcter vector of the labels you want on the data frame
#' @param add a Boolean that says weather you want to add new labels rather than replace them
#'
#' @return a data frame with new changed labels
#' @export
#'
#' @examples
replace_labels <- function(data, new_labels, add = FALSE){ #can maybe make this functional - faster/less memory

  labels <- data %>%
    get_labels() # gets current labels


  for(i in seq_along(new_labels)){
    if((!is.null(labels[[i]]) )| add == TRUE)  #checks if the current label is empty or user wants to add a label regardless
      attr(data[[i]], "label") <- new_labels[[i]] #replaces old label with new one
    else
      attr(data[[i]], "label") <- "" #replaces null labels with empty_string so the df name doesn't row e.g(COL3)
  }
  data #returns df with new labels
}

#' Get dataframe labels
#'
#' @param data the data frame for which you want to get the labels
#'
#' @return a character vector of the labels you want
#' @export
#'
#' @examples
get_labels <- function(data){
  data %>%
    lapply(function(x) attr(x, "label")) # gets current labels
}




apply_labels <- function(ht, labels){
  if(length(labels) > 0){
    for(i in seq_along(labels)){
      colname <- names(labels)[i]
      lbls <- labels[[colname]]
      #ht <- ht %>%

    }
  }
}
