
#orders columns in right order
#' Orders data and descriptor columns into the correct order
#'
#' @param data a data frame containing descriptor and data containing columns
#'
#' @return a data frame containing descriptor and data containing columns in the correct order
#' @importFrom dplyr relocate all_of
#' @keywords internal
#'
order_cols <- function(data){

  ROWLBLs <- data %>%
    get_col_names(houdini_global$defaults$rowlbls.name)

  COLs <- data %>%
    get_col_names(houdini_global$defaults$cols.name)

  for(i in length(COLs):1){
    column_name <- sprintf("%s%s", houdini_global$defaults$cols.name, i)
    data <- data %>%
      dplyr::relocate(dplyr::all_of(column_name))
  }
  for(i in length(ROWLBLs):1){
    column_name <- sprintf("%s%s", houdini_global$defaults$rowlbls.name, i)
    data <- data %>%
      dplyr::relocate(dplyr::all_of(column_name))
  }
  data
}


#' Removes repeated data within a chunk e.g, AAAABBBB -> A   B
#'
#' @param col a list representing the column for which the data needs to be separated
#'
#' @return a list with the data separated
#' @keywords internal
#'
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
#' Wrapper function to split footnotes before calling the houdinitable add_footer() function
#'
#' @param ht a houdinitable object
#' @param footnotes an un-split string of all the footnotes for a table
#'
#' @return a houdinitable object with footnotes added
#' @keywords internal
#'
add_footnote <- function(ht, footnotes = ""){
  #if there are no footnotes return table as is
  if(footnotes == ""){
    footnotes <- c()
  }else{
    #splits footnotes into a vector by delimiter
    footnotes <- footnotes %>%
      strsplit(split = houdini_global$defaults$delimiter) %>%
      unlist()
  }



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


#' Title
#' @importFrom dplyr select all_of starts_with
#' @importFrom tibble add_row
#' @importFrom stringr str_count
#' @keywords internal
#'
add_row_buffers2 <- function(data){

  #adds no buffers if theres one row or less
  if(nrow(data) < 2)
    data

  #gets labels as for some reason adding rows removes them
  labels <- data %>%
    get_labels()
  pattern <- sprintf("%s1", houdini_global$defaults$rowlbls.name)
  row_grps <- data %>%
    dplyr::select(all_of(starts_with(houdini_global$defaults$rowlbls.name))) %>%
    apply(c(1,2),function(x){
      if(x == ""){
        return(1000)
      }
      else{
        return(stringr::str_count(x,"^\\s+"))
      }
    }) %>%
    data.frame()
  row_grps[names(row_grps) != pattern] <-  row_grps[names(row_grps) != pattern] %>%
    apply(c(1,2),function(x){
      if(x == 1000){
        return(0)
      }
      return(x)
    })
  if(all(row_grps == 0)){
    condition <- function(lhs,rhs){
      return(any(lhs < rhs))
    }
  }else{
    condition <- function(lhs,rhs){
      return((any(lhs < rhs)) || (lhs[1] == 0 && rhs[1] == 0))
    }
  }

  rows_added <- 0
  for(i in seq_len(nrow(row_grps))){
    if( i == 1){
      data <- data %>%
        tibble::add_row(.before = (i+rows_added))
      rows_added <- rows_added +1
      prev_row <- row_grps[i,]
    }else{
      if(condition(row_grps[i,], prev_row)){
        data <- data %>%
          tibble::add_row(.before = (i+rows_added))
        rows_added <- rows_added +1
      }
      prev_row <- row_grps[i,]
    }
  }
  #replaces labels as for some reason they dissapear when adding rows
  data <- data %>%
    replace_labels(labels, add = TRUE)

  #returns data with buffer rows added
  data
}


#' Adds buffer rows (row spacing) between groups of rows
#'
#' @param data a data frame to have buffer rows added to it
#'
#' @return a data frame with buffer rows added to it
#' @importFrom dplyr select all_of starts_with
#' @importFrom tibble add_row
#' @importFrom stringr str_count
#' @keywords internal
#'
add_row_buffers <- function(data){

  #adds no buffers if theres one row or less
  if(nrow(data) < 2)
    data

  #gets labels as for some reason adding rows removes them
  labels <- data %>%
    get_labels()

  #gets the set name of the descriptor containing columns and the column itself
  col_label <- sprintf("%s1", houdini_global$defaults$rowlbls.name)
  first_col <- data[[col_label]]

  #gets the index of rows where the indent goes backwards or it goes from an empty cell to not
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

  #adds in empty rows at these indexes
  rows_added = 0
  for(i in groups){
    data <- data %>%
      tibble::add_row(.before = (i+rows_added))
    rows_added <- rows_added +1
  }

  #replaces labels as for some reason they dissapear when adding rows
  data <- data %>%
    replace_labels(labels, add = TRUE)

  #returns data with buffer rows added
  data
}




#' Adds buffer columns (column spacing) between groups of columns
#'
#' @param data a data frame to have buffer columns added to it
#'
#' @return a data frame with buffer columns added to it
#' @importFrom tibble add_column
#' @keywords internal
#'
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
      append(" ", after = i+cols_added) #adds new blank column label to buffer columns

    cols_added <- cols_added + 1
  }

  #reapplys labels and returns data frame with added buffer columns
  data <- data %>%
    replace_labels(labels, add = TRUE)
  data
}

#adds a column that shows which "chunk" of data each row belongs
#' Adds a column that shows which "chunk of data each row belongs do
#' This is done by grouping data between buffers into the same "chunk"
#'
#' @param data a data frame to have the page column appended
#'
#' @return a data frame with the page column appended
#' @keywords internal
#'
add_page_column <- function(data){

  #gets labels as adding a column appears to delete them
  labels <- data %>%
    get_labels()

  #adds an extra label for the new column
  labels <- labels %>%
    append("PAGE")

  col_label <- sprintf("%s1", houdini_global$defaults$rowlbls.name)
  first_col <- data[[col_label]]

  #gets the index of rows where the indent goes backwards or it goes from an empty cell to not
  prev_elem <- ""
  last_na <- FALSE
  groups <- integer()
  for(i in seq_along(first_col)){
    elem <- first_col[i]
    if(is.na(elem)){
      elem <- prev_elem
      last_na <- TRUE
      next
    }
    elem_indent <- elem %>%
      stringr::str_count("^\\s+")
    prev_indent <- prev_elem %>%
      stringr::str_count("^\\s+")
    if((prev_elem == "" && elem != "") || (elem_indent < prev_indent) || (prev_indent == 0 && elem_indent == 0 && elem != "" && prev_elem != elem)){
      if(last_na){
        groups <- c(groups,i-1)
      }else{
        groups <- c(groups,i)
      }

    }
    last_na <- FALSE
    prev_elem <- elem
  }

  PAGE <- c()
  page_num <- 0
  for(i in 1:length(first_col)){ #iterates through each row of first column
    if(i %in% groups){ #if it reaches a row found earlier increment the page number
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




#' Filters data by certain parameters e.g. score
#'
#' @param data a data frame representing the data to be filtered
#' @param parameter a character vector containing parameters to be filtered by
#'
#' @return a filtered data frame
#' @importFrom purrr is_empty
#' @importFrom dplyr filter select where
#' @importFrom logger log_warn
#' @keywords internal
#'
parameter_filtering <- function(data, parameter){
  if(purrr::is_empty(parameter))
  {
    return(data)
  }

  # pattern <- sprintf("%s1",houdini_global$defaults$param.name)
  # if(!(pattern %in% names(data))){
  #   return(data)
  # }

  pattern <- data %>%
    select(where(~ any(. %in% parameter, na.rm = TRUE)))
  if(ncol(pattern) < 1){
    return(data)
    print("None found")
  }
  pattern <- names(pattern)

  parameter <- parameter %>%
    sapply(function(x){
      if(x %in% data[[pattern]])
      {
        x
      }
      else{
        logger::log_warn("Parameter filter: {x} not found - parameter filter not applied", namespace = "Houdini Log")
        c()
      }
    }) %>%
    unlist()
  parameter <- parameter[!is.null(parameter)]
  for(i in seq_along(parameter)){
    logger::log_info("Parameter filter: {parameter[i]} applied", namespace = "Houdini Log")
  }

  new_data <- data %>%
    filter(.data[[pattern]] %in% parameter)


  if(nrow(new_data) == 0){
    return(data)
  }
  return(new_data)
}

#' Filters data by the timeline that they belong to
#'
#' @param data a data frame representing the data to be filtered
#' @param parameter a character vector containing timelines to be filtered by
#'
#' @return a filtered data frame
#' @importFrom purrr is_empty
#' @importFrom dplyr filter
#' @importFrom logger log_warn log_info
#' @keywords internal
#'
timeline_filtering <- function(data, parameter){
  if(purrr::is_empty(parameter))
  {
    return(data)
  }


  first_col_name <- sprintf("%s1", houdini_global$defaults$rowlbls.name)
  parameter <- parameter %>%
    sapply(function(x){
      if(x %in% data[[first_col_name]])
      {
        x
      }
      else{
        logger::log_warn("Timeline filter: {x} not found - timeline filter not applied", namespace = "Houdini Log")
        c()
      }
    }) %>%
    unlist()
  parameter <- parameter[!is.null(parameter)]
  for(i in seq_along(parameter)){
    logger::log_info("Timeline filter: {parameter[i]} applied", namespace = "Houdini Log")
  }
  page_group <- data %>%
    dplyr::filter(.data[[first_col_name]] %in% parameter) %>%
    with(PAGE)

  new_data <- data %>%
    dplyr::filter(PAGE %in% page_group)

  if(nrow(new_data) == 0){
    return(data)
  }
  return(new_data)

}


