
#replaces old labels with new ones
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

get_labels <- function(data){
  data %>%
    lapply(function(x) attr(x, "label")) # gets current labels
}
