













add_buffers <- function(data){ # having issues with dropping labels - got a work around but its not ideal

  labels <- data %>%
    get_labels()

  groups <- get_groups(data)

  cols_added <- 0
  for(i in groups){
    col_name = sprintf("BUFFER%i",cols_added+1)
    data <- data %>%
      tibble::add_column(!!col_name := NA , .after = i+cols_added) #adds empty buffer column after data columns - cols_added accounts for added new ones

    labels <- labels %>%
      append("   ", after = i+cols_added) #adds new blank column label to buffer columns

    cols_added <- cols_added + 1
  }


  first_col <- data[[1]]
  rows_added <- 0
  for(i in 1:length(first_col)){
    if(!grepl("^\\s\\s", first_col[i]) && first_col[i] != ""){

      data <- data %>%
        tibble::add_row(.before = i+rows_added)

      rows_added <- rows_added + 1
    }
   }


  data <- data %>%
    replace_labels(labels, add = TRUE)
  data
}







