
#orders columns in right order
order_cols <- function(data){

  ROWLBLs <- data %>%
    get_col_names(houdini_global$defaults$rowlbls.name)

  COLs <- data %>%
    get_col_names(houdini_global$defaults$cols.name)

  for(i in length(COLs):1){
    column_name <- sprintf("%s%s", houdini_global$defaults$cols.name, i)
    data <- data %>%
      relocate(column_name)
  }
  for(i in length(ROWLBLs):1){
    column_name <- sprintf("%s%s", houdini_global$defaults$rowlbls.name, i)
    data <- data %>%
      relocate(column_name)
  }
  data
}

#removes repeated values within a chunk of data
separate_data <- function(col){
  #if its smaller than 2 there can be no repeated values
  if(length(col) < 2)
  {
    col
  }
  else
  {
    prev_elem <- col[1] #gets first element
    for(i in 2:length(col)){ #checks each element against the last if theyre the same remove that element
      if(col[i] != "" && !is.na(col[i]))
      {
        if(col[i] == prev_elem)
          col[i] <- ""
        else
          prev_elem <- col[i]
      }
    }
    #return formatted column
    col
  }


}

#adds footnotes to a table
add_footnote <- function(ht, footnotes = ""){
  #if there are no footnotes return table as is
  if(footnotes == "")
    ht

  #splits footnotes into a vector by delimiter
  footnotes <- footnotes %>%
    strsplit(split = houdini_global$defaults$delimiter) %>%
    unlist()



  #returns table with footer rows added
  ht <- ht %>%
    add_footer(footnotes)
  ht
}

format_sizes <- function(ft,doc_width,chr_cols,data_cols,buffer_cols){

  desciptor_col_ratio <- 2

  col_width <- doc_width / (length(chr_cols)*desciptor_col_ratio + length(data_cols) + (length(buffer_cols))*0.2)

  descriptor_col_width <- col_width

}



add_row_buffers <- function(data){

  if(nrow(data) < 2)
    data

  labels <- data %>%
    get_labels()


  col_label <- sprintf("%s1", houdini_global$defaults$rowlbls.name)
  first_col <- data[[col_label]]
  groups <- prev_apply(first_col, "", f = function(elem, prev_elem, counter){
    elem_indent <- elem %>%
      stringr::str_count("^\\s+")
    prev_indent <- prev_elem %>%
      stringr::str_count("^\\s+")
    if((prev_elem == "" && elem != "") || (elem_indent < prev_indent) || (prev_indent == 0 && elem_indent == 0 && elem != "" && prev_elem != elem)){
      counter
    }
    else
      c()
  })

  rows_added = 0
  for(i in groups){
    data <- data %>%
      tibble::add_row(.before = (i+rows_added))
    rows_added <- rows_added +1
  }

  data <- data %>%
    replace_labels(labels, add = TRUE)
  data
}




add_col_buffers <- function(data){ # having issues with dropping labels - got a work around but its not ideal

  #get labels as adding columns deletes them
  labels <- data %>%
    get_labels()

  #get groups of columns e.g. columns with the same second row header
  groups <- get_groups(data)

  #adds buffer columns in after each "group" of column
  cols_added <- 0
  for(i in groups){
    col_name = sprintf("BUFFER%i",cols_added+1)
    data <- data %>%
      tibble::add_column(!!col_name := NA , .after = i+cols_added) #adds empty buffer column after data columns - cols_added accounts for added new ones

    labels <- labels %>%
      append("   ", after = i+cols_added) #adds new blank column label to buffer columns

    cols_added <- cols_added + 1
  }

  #reapplys labels and returns data frame with added buffer columns
  data <- data %>%
    replace_labels(labels, add = TRUE)
  data
}

#adds a column that shows which "chunk" of data each row belongs
add_page_column <- function(data){

  #gets labels as adding a column appears to delete them
  labels <- data %>%
    get_labels()

  #adds an extra label for the new column
  labels <- labels %>%
    append("PAGE")

  #gets first column to base groups off
  first_col = data[[1]]

  PAGE <- c()
  page_num <- 0
  for(i in 1:length(first_col)){ #iterates through each row of first column
    if(is.na(first_col[i])){ #if it reaches a buffer row place a buffer in the new column and increment the page number
      page_num <- page_num + 1
      PAGE <- PAGE %>%
        append(page_num)
    }
    else #otherwise just append the current page number to the new column
    {
      PAGE <- PAGE %>%
        append(page_num)
    }
  }

  #add new column to data frame
  data <- data %>%
    cbind(PAGE)

  #reapply labels as they were deleted and return data frame with new column
  data <- data %>%
    replace_labels(labels, add = TRUE)

  data

}





parameter_filtering <- function(data, parameter){
  if(purrr::is_empty(parameter))
  {
    return(data)
  }
  pattern <- sprintf("%s1",houdini_global$defaults$param.name)
  if(!(pattern %in% names(data))){
    return(data)
  }
  new_data <- data %>%
    filter(.data[[pattern]] == parameter) #%>%		# its likely the column will change
    #mutate(!!sym(houdini_global$defaults$param.name) = NULL)
  if(nrow(new_data) == 0){
    return(data)
  }
  else{
    return(new_data)
  }
}

timeline_filtering <- function(data, parameter){
  if(purrr::is_empty(parameter))
  {
    return(data)
  }


  first_col_name <- sym(sprintf("%s1", houdini_global$defaults$rowlbls.name))

  page_group <- data %>%
    filter(.data[[first_col_name]] %in% parameter) %>%
    with(PAGE)

  new_data <- data %>%
    filter(PAGE %in% page_group)

  if(nrow(new_data) == 0){
    return(data)
  }
  else{
    return(new_data)
  }
}


