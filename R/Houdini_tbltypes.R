#Here temporarily for testing
std_labels = c(A = "Part A (Placebo Controlled)", B = "Part B (Maintainence)")
header_border = officer::fp_border(color = "black" , width = 1.5)


shift_table <- function(data){
  grouped_data <- data %>%
    group_by(BYGRP,BYGRPN,ROWGRP1,ROWGRP2)
  temp <- grouped_data %>%
    summarise(
      N = n()
    )
}


apply_second_header <- function(data,second_labels,header_code){


  #only runs if there are secondary headers
  if(!purrr::is_empty(header_code)){

    #gets label attribute for each column
    labels <- data %>%
      lapply(function(x) attr(x, "label"))

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


standard_format <- function(data,header_code, delimiter = "\\^\\*\\^"){

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
    apply_second_header(std_labels,header_code) %>%
    add_buffers()

  #gets label attribute for each column
  labels <- data %>%
    get_labels()

  #gets second layer of headers by delimiter
  second_headers <- labels %>%
    sapply(function(x){
      if(grepl(delimiter,x)){
        strsplit(x,delimiter, fixed = FALSE)[[1]][1]
      }
      else
        ""
    })
  #gets first row of headers without second row appended
  first_headers <- labels %>%
    sapply(function(x){
      if(grepl(delimiter,x)){
        strsplit(x,delimiter, fixed = FALSE)[[1]][2]
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
    align(align = c("left"), part = "all", j = chr_cols)  #aligns descriptor columns to the left
  ft

}

#brings secondary headers marked by the ^*^ delimiter up to a second row of headers
lift_headers <- function(ft,second_headers){

  border_cols <- second_headers %>%
    sapply(function(x){

      if(x == ""){
        FALSE
      }
      else
        TRUE
    }) %>%
    which()

  ft <- ft %>%
    add_header_row(values = second_headers, top = TRUE ) %>%
    border_remove() %>%
    hline(j=border_cols,border = header_border,part = "header") %>%
    hline(i = 2,part = "header", border = header_border) %>%
    hline_top(part = "header", border = header_border) %>%
    hline_bottom(part = "body" , border = header_border) %>%
    merge_h(part = "header")

  ft
}

change_from_baseline <- function(data){

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
  if(!purrr::is_empty(LBLINDENTs))
    ROWLBLs <- mapply(indent,ROWLBLs,LBLINDENTs)

  first_headers <- data$COLVAR2
  second_headers <- data$COLVAR1
  new_headers <- merge_columns(second_headers,first_headers, separator = "^*^")
  labels <- c(labels,unique(new_headers))


  CELLVALs <- data$CELLVALC2
  n_points <- new_headers %>%
    unique() %>%
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
      new_row[col_name] <- data[[col_name]][i+j]
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



#combines two columns of strings
merge_columns <- function(col1,col2,separator = ""){
  mapply(function(cell1,cell2){
    if(cell1 !="")
      sprintf("%s%s%s",cell1,separator,cell2)
    else
      cell2
  },col1,col2)

}

#indents a label column with its specified indents
indent <- function(column, indent_column){
  indents <- indent_column %>%
    replace(is.na(.), 0) %>%
    strrep(" ", .)

  merge_columns(indents,column)
}





