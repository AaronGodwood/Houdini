




#' Formats standard formatted data sets into tables
#'
#' 1-Apply and secondary layers of headers from TRTLBL columns if they exist
#' 2-Filter data by parameter
#' 3-Filter first descriptor column so that repeated data is not shown
#' 4-Order columns
#' 5-If there is currently no secondary layer of headers apply the one provided by excel document
#' 6-Add row & column buffers
#' 7-Add a PAGE column that groups rows - used for filtering
#' 8-Get column names you want to show up
#' 9 - Filter by timelines
#' 10-Convert dataset to houdinitable
#' 11-Apply central alignment to data columns & left alignment to descriptor columns
#'
#' @param data a data frame that represents a standard format dataset
#' @param header_code a header code for if the dataset is missing a layer of headers
#' @param doc_width a float that represents the width of a word doc
#' @param filters a named vector produced by sort_filters() that describes the filtering required
#'
#' @return a formatted table representing the data from the data frame
#' @importFrom dplyr select starts_with where
#' @importFrom lubridate setdiff
#' @keywords internal
#'
#'
standard_format <- function(data, header_code = NULL, doc_width = 6.5, filters = c(parameters = c(), timelines = c() )){



  #gets delimiter for splitting headers and footers from global Houdini settings
  delimiter <- houdini_global$defaults$delimiter

  #if the columns labelled TRTLBL12 ect exist apply their second row of headers to the dataset
  pattern <- sprintf("%s1", houdini_global$defaults$trtlbls.name)
  if( any( sapply( names( data), function( x) {grepl( pattern, x)}))){
    data <- data %>%
      apply_trt_headers()
  }

  #gets only columns that are descriptor column or data columns after filtering by parameters
  # applying the second layer of headers and parameter filtering must be done before selecting columns as the columns used to do this get deleted at this point
  data <- data %>%
    parameter_filtering(filters$parameters) %>%
    dplyr::select(starts_with(c( houdini_global$defaults$cols.name, houdini_global$defaults$rowlbls.name,houdini_global$defaults$rowgrp.name ))) # this should probably be moved at some point when i've worked out what filtering i will do

  #removes repeated row labels within the same "chunk" e.g. WeeK 1, Week 1, Week 1 goes to Week 1
  first_col_lbl <- sprintf("%s1",houdini_global$defaults$rowlbls.name)
  data[[first_col_lbl]] <- data[[first_col_lbl]] %>%
    separate_data()


  #Orders columns with descriptor cols on left followed by data cols in order - must happen before subsequent code as they rely on ordered cols
  data <- data %>%
    add_row_buffers2() %>%
    dplyr::select(starts_with(c( houdini_global$defaults$cols.name, houdini_global$defaults$rowlbls.name))) %>%
    order_cols()

  #checks if any of the header labels already have a second row of labels by checking if they contain the delimiter that will be used to split them later
  if(!any( sapply( get_labels(data), function(x) grepl(delimiter,x)))){
    #if this is not the case apply the second layer of headers provided by the header code from the excel doc
    data <- data %>%
      apply_second_header(header_code)
  }

  #gets descriptor column names so these can be aligned left later
  chr_cols <- data %>%
    dplyr::select(dplyr::where( ~ any(grepl("[A-Za-z]", .)))) %>%
    names()

  #gets data columns names so that these can be aligned centrally later
  data_cols <- data %>%
    names() %>%
    lubridate::setdiff(chr_cols)

  #adds buffers between row groups of data and between column groups then adds a page column that describes what groups each row belongs to
  #page column must be added after row buffers as it rely s on the row buffers to categorise the groups
  # I undertand that dataset starts with a similar column however it was done this way as datasets were inconsistent
  data <- data %>%
    add_col_buffers() %>%
    add_page_column()


  #gets the buffer columns names
  buffer_cols <- data %>%
    names() %>%
    lubridate::setdiff(union(chr_cols,data_cols))


  #gets column names without the PAGE column so they don't show later on as houdinitable only shows the column of the colkeys you provide
  colkeys <- data %>%
    names()
  colkeys <- colkeys[-length(colkeys)]

  #filters data by timeline - requires the page column to have been added
  data <- data %>%
    timeline_filtering(filters$timelines)

  #turns dataset into a houdinitable
  ht <- data %>%
    houdinitable(col_keys = colkeys) %>%
    set_alignments("center") %>%
    set_alignments("start", chr_cols) %>%
    paginate(data[["PAGE"]]) %>%
    auto_widths(data_cols)



  ht

}






#' Formats non-standard formatted data sets into standard formatted ones
#'
#' @param data a data frame that represents the non-standard formatted dataset
#'
#' @return a data frame that represents a standard formatted dataset
#' @importFrom dplyr select starts_with
#' @importFrom purrr is_empty
#' @importFrom tibble tibble
#' @keywords internal
#'
#'
non_standard_format <- function(data){


  #gets all the row label columns
  ROWLBLs <- data %>%
    get_col_names(houdini_global$defaults$rowlbls.name)

  #gets the column labels from the row label columns
  labels <- ROWLBLs %>%
    get_labels()

  #gets the number of row label columns
  n_lbls <- ROWLBLs %>%
    length()

  #gets the row label indent columns if they exist
  indent_pattern <- sprintf("^%s[0-9]INDENTS$", houdini_global$defaults$rowlbls.name)
  LBLINDENTs <- data %>%
    dplyr::select(grep(indent_pattern,names(.)))        #URGENT THIS WORKS NOW BUT UNDERMINES GLOBAL SETTINGS PURPOSE
  #if row label indent columns exist apply indents
  if(!(purrr::is_empty(LBLINDENTs))){
    for(i in 1:length(ROWLBLs))
      ROWLBLs[[i]] <- indent(ROWLBLs[[i]],LBLINDENTs[[i]])
  }

  headers <- data %>%
    get_col_names(houdini_global$defaults$colvars.name)
  current_headers <- rep("",nrow(headers))
  for(i in seq_along(headers)){
    column_name <- sprintf("%s%s",houdini_global$defaults$colvars.name,i)
    current_header <- headers[[column_name]]
    if(i == length(headers)){
      current_header <- current_header %>%
        fill_gaps()
    }
    current_headers <- merge_columns(current_headers,current_header, separator = houdini_default_settings$delimiter.non.regex)
  }
  new_headers <- current_headers %>%
    unique()


  labels <- c(labels,new_headers)


  CELLVALs <- data %>%
    dplyr::select(dplyr::starts_with( c( houdini_global$defaults$cellvalcs.name )))
  CELLVALs <- CELLVALs[[1]]
  n_points <- new_headers %>%
    length()


  new_data <- tibble()
  for(i in 1:n_points)
  {
    if(i <= n_lbls){
      col_name <- col_name <- sprintf("%s%s", houdini_global$defaults$rowlbls.name , i)
      new_data[col_name] = character()
    }
    col_name <- sprintf("%s%s", houdini_global$defaults$cols.name , i)
    new_data[col_name] = character()
  }
  for(i in seq(1,length(rownames(data)),by = n_points)){
    new_row = list()

    for(j in 0:(n_lbls-1)){
      col_name <- sprintf("%s%s", houdini_global$defaults$rowlbls.name ,j+1)
      new_row[col_name] <- ROWLBLs[[col_name]][i+j]
    }

    for(j in 0:(n_points-1)){
      col_name <- sprintf("%s%s", houdini_global$defaults$cols.name , j+1)
      new_row[col_name] <- CELLVALs[i+j]
    }
    new_data <- new_data %>%
      rbind(new_row)
  }

  #adds correct labels to the formatted data
  new_data <- new_data %>%
    replace_labels(labels, add = TRUE)


  new_data
}







