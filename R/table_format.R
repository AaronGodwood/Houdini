#Here temporarily for testing
std_labels = c(A = "Part A (Placebo Controlled)", B = "Part B (Maintainence)")
header_border = officer::fp_border(color = "black" , width = 1.5)



#' Applies default formatting to a table
#'
#' @param ft a flextable for formatting to be applied to
#'
#' @return a flextable with formatting applied
#' @export
#'
#' @examples
apply_flextable_defaults <- function(ft) {
  ft <- ft %>%
    fontsize(size = 10) %>%             # Set font size to 10
    fontsize(size = 10, part = "header") %>%
    font(font = "Times New Roman") %>%   # Set font to Times New Roman
    font(font = "Times New Roman", part = "header") %>%   # Set font to Times New Roman
    bold(part = "header") %>%  # set header to bold
    padding(padding = 0) %>%            # Set padding to 0
    line_spacing(space = 1) #%>%          # Set line spacing to 1
    #set_table_properties(width = 1,layout = "autofit")  # Autofit the table layout
  return(ft)
}


#' Formats standard formatted data sets into tables
#'
#' @param data a data frame that represents a standard format dataset
#' @param header_code a header code for if the dataset is missing a layer of headers
#' @param delimiter a delimiter fro splitting flattened headers back into layered ones
#'
#' @return a formatted table representing the data from the data frame
#' @export
#'
#' @examples
standard_format <- function(data, header_code = NULL, doc_width = 6.5, delimiter = "\\^\\*\\^"){

  data <- data %>%
    dplyr::select(starts_with(c("COL", "ROWLBL"))) # this should probably be moved at some point when i've worked out what filtering i will do

  data$ROWLBL1 <- data$ROWLBL1 %>%
    separate_data()
  #gets descriptor columns
  chr_cols <- data %>%
    dplyr::select(dplyr::where( ~ any(grepl("[A-Za-z]", .)))) %>%
    names()

  #gets data columns
  data_cols <- data %>%
    names() %>%
    lubridate::setdiff(chr_cols)



  #formats data with secondary headers and buffer rows/cols
  data <- data %>%
    order_cols() %>%
    apply_second_header(std_labels,header_code) %>%

    add_buffers()

  buffer_cols <- data %>%
    names() %>%
    lubridate::setdiff(union(chr_cols,data_cols))



  #gets label attribute for each column
  labels <- data %>%
    get_labels()

  #gets second & higher layer of headers by delimiter
  second_headers <- labels %>%
    sapply(function(x){
      if(grepl(delimiter,x)){
        headers <- strsplit(x,delimiter, fixed = FALSE)[[1]]
        headers[-(length(headers))]
      }
      else
        ""
    })
  #gets first row of headers without second row appended
  first_headers <- labels %>%
    sapply(function(x){
      if(grepl(delimiter,x)){
        tail(strsplit(x,delimiter, fixed = FALSE)[[1]],1)
      }
      else
        x
    })

  #formats first row of header to only contain first row labels
  data <- data %>%
    replace_labels(first_headers)

  ft <- data %>%
    dplyr::select(-starts_with(c("ROWORD", "PAGE"))) %>%
    flextable() %>%
    apply_flextable_defaults() %>%
    lift_headers(second_headers) %>%
    align(align = c("center"), part = "all") %>%  #align all columns (to be just data columns) centrally
    align(align = c("left"), part = "all", j = chr_cols) %>% #aligns descriptor columns to the left
    set_table_properties(width = 1,layout = "autofit") %>% # Autofit the table layout
    width(j = chr_cols, width = (col_width*desciptor_col_ratio)) %>%
    width(j = buffer_cols, width = (col_width* 0.2)) %>%
    width(j = data_cols, width = col_width)
  ft

}

order_cols <- function(data){
  ROWLBLs <- data %>%
    select(grep("^ROWLBL[0-9]$",names(.)))
  COLs <- data %>%
    select(grep("^COL[0-9]$",names(.)))
  for(i in length(COLs):1){
    column_name <- sprintf("COL%s",i)
    data <- data %>%
      relocate(column_name)
  }
  for(i in length(ROWLBLs):1){
    column_name <- sprintf("ROWLBL%s",i)
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
      if(col[i] != "")
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


format_sizes <- function(ft,doc_width,chr_cols,data_cols,buffer_cols){

  desciptor_col_ratio <- 2

  col_width <- doc_width / (length(chr_cols)*desciptor_col_ratio + length(data_cols) + (length(buffer_cols))*0.2)

  descriptor_col_width <- col_width

}

#' Formats non-standard formatted data sets into standard formatted ones
#'
#' @param data a data frame that represents the non-standard formatted dataset
#'
#' @return a data frame that represents a standard formatted dataset
#' @export
#'
#' @examples
non_standard_format <- function(data){

  #gets all the row label columns
  ROWLBLs <- data %>%
    select(grep("^ROWLBL[0-9]$",names(.)))

  #gets the column labels from the row label columns
  labels <- ROWLBLs %>%
    get_labels()

  #gets the number of row label columns
  n_lbls <- ROWLBLs %>%
    length()

  #gets the row label indent columns if they exist
  LBLINDENTs <- data %>%
    select(grep("^ROWLBL[0-9]INDENT$",names(.)))
  #if row label indent columns exist apply indents
  if(!(purrr::is_empty(LBLINDENTs))){
    for(i in 1:length(ROWLBLs))
      ROWLBLs[[i]] <- indent(ROWLBLs[[i]],LBLINDENTs[[i]])
  }

  headers <- data %>%
    select(grep("^COLVAR[0-9]$",names(.)))
  #gets rows of headers
  first_headers <- headers %>%
    dplyr::select(dplyr::last_col())
  first_headers <- first_headers[[1]] %>%
    fill_gaps()
  if(length(headers) > 1){
    second_headers <- data$COLVAR1
    new_headers <- merge_columns(second_headers,first_headers, separator = "^*^") %>%
      unique()
  }
  else
    new_headers <- first_headers %>%
      unique()

  labels <- c(labels,new_headers)


  CELLVALs <- data %>%
    dplyr::select(starts_with(c("CELLVALC")))
  CELLVALs <- CELLVALs[[1]]
  n_points <- new_headers %>%
    length()


  new_data <- tibble()
  for(i in 1:n_points)
  {
    if(i <= n_lbls){
      col_name <- col_name <- sprintf("ROWLBL%s", i)
      new_data[col_name] = character()
    }
    col_name <- sprintf("COL%s", i)
    new_data[col_name] = character()
  }
  for(i in seq(1,length(rownames(data)),by = n_points)){
    new_row = list()

    for(j in 0:(n_lbls-1)){
      col_name <- sprintf("ROWLBL%s", j+1)
      new_row[col_name] <- ROWLBLs[[col_name]][i+j]
    }

    for(j in 0:(n_points-1)){
      col_name <- sprintf("COL%s", j+1)
      new_row[col_name] <- CELLVALs[i+j]
    }
    new_data <- new_data %>%
      rbind(new_row)
  }
  #adds correct labels to the formatted data
  new_data <- new_data %>%
    replace_labels(labels, add = TRUE)
}





