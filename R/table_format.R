#Here temporarily for testing
std_labels = c(A = "Part A (Placebo Controlled)", B = "Part B (Maintainence)")
header_border = officer::fp_border(color = "black" , width = 1.5)



apply_flextable_defaults <- function(ft) {
  ft <- ft %>%
    fontsize(size = 10) %>%             # Set font size to 10
    fontsize(size = 10, part = "header") %>%
    font(font = "Times New Roman") %>%   # Set font to Times New Roman
    font(font = "Times New Roman", part = "header") %>%   # Set font to Times New Roman
    bold(part = "header") %>%  # set header to bold
    padding(padding = 0) %>%            # Set padding to 0
    line_spacing(space = 1) %>%          # Set line spacing to 1
    set_table_properties(width = 1,layout = "autofit")  # Autofit the table layout
  return(ft)
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
    align(align = c("left"), part = "all", j = chr_cols)  #aligns descriptor columns to the left
  ft

}



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


  #gets rows of headers
  first_headers <- data$COLVAR2 %>%
    fill_gaps()
  second_headers <- data$COLVAR1
  new_headers <- merge_columns(second_headers,first_headers, separator = "^*^") %>%
    unique()
  labels <- c(labels,new_headers)


  CELLVALs <- data$CELLVALC2
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





