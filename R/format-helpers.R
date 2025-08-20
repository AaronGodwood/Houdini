
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

separate_data <- function(col){
  if(length(col) < 2)
  {
    col
  }
  else
  {
    prev_elem <- col[1]
    for(i in 2:length(col)){
      if(col[i] != "" && !is.na(col[i]))
      {
        if(col[i] == prev_elem)
          col[i] <- ""
        else
          prev_elem <- col[i]
      }
    }
    col
  }


}

add_footnote <- function(ft, footnotes = ""){
  if(footnotes == "")
    ft

  footnotes <- footnotes %>%
    strsplit(split = houdini_global$defaults$delimiter)

  footnotes <- footnotes[[1]]

  ft %>%
    add_footer_lines(values = footnotes)

}

format_sizes <- function(ft,doc_width,chr_cols,data_cols,buffer_cols){

  desciptor_col_ratio <- 2

  col_width <- doc_width / (length(chr_cols)*desciptor_col_ratio + length(data_cols) + (length(buffer_cols))*0.2)

  descriptor_col_width <- col_width

}



add_row_buffers <- function(data){

  labels <- data %>%
    get_labels()


  col_label <- sprintf("%s1", houdini_global$defaults$rowlbls.name)
  first_col <- data[[col_label]]
  groups <- c()
  prev_elem <- ""
  prev_indent <- 0
  for(i in 1:length(first_col)){
    indent_count <- first_col[i] %>%
      stringr::str_count("^\\s+")
    if((prev_elem == "" && first_col[i] != "") || (indent_count < prev_indent) || (prev_indent == 0 && indent_count == 0 && first_col[i] != "" && prev_elem != first_col[i])){
      groups <- groups %>%
        append(i)
    }
    prev_elem <- first_col[i]
    prev_indent <- indent_count
  }
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

  data <- data %>%
    replace_labels(labels, add = TRUE)
  data
}

add_page_column <- function(data){

  labels <- data %>%
    get_labels()

  labels <- labels %>%
    append("PAGE")

  first_col = data[[1]]
  PAGE <- c()
  page_num <- 1
  for(i in 1:length(first_col)){
    if(is.na(first_col[i])){
      PAGE <- PAGE %>%
        append(NA)
      page_num <- page_num + 1
    }
    else
    {
      PAGE <- PAGE %>%
        append(page_num)
    }
  }

  data <- data %>%
    cbind(PAGE)

  data <- data %>%
    replace_labels(labels, add = TRUE)

  data

}
