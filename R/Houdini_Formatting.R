






#replaces old labels with new ones
replace_labels <- function(data, new_labels, add = FALSE){ #can maybe make this functional - faster/less memory

  labels <- data %>%
    lapply(function(x) attr(x, "label")) # gets current labels


  for(i in seq_along(new_labels)){
    if((!is.null(labels[[i]]) )| add == TRUE)  #checks if the current label is empty or user wants to add a label regardless
      attr(data[[i]], "label") <- new_labels[[i]] #replaces old label with new one
    else
      attr(data[[i]], "label") <- "" #replaces null labels with empty_string so the df name doesn't row e.g(COL3)
  }
  data #returns df with new labels
}


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

get_labels <- function(data){
  data %>%
    lapply(function(x) attr(x, "label")) # gets current labels
}


add_buffers <- function(data){ # having issues with dropping labels - got a work around but its not ideal

  labels <- data %>%
    get_labels()

  groups <- get_groups(data)

  cols_added <- 0
  for(i in groups){
    col_name = sprintf("BUFFER%i",cols_added+1)
    data <- data %>%
      add_column(!!col_name := NA , .after = i+cols_added) #adds empty buffer column after data columns - cols_added accounts for added new ones

    labels <- labels %>%
      append("   ", after = i+cols_added) #adds new blank column label to buffer columns

    cols_added <- cols_added + 1
  }



  first_col <- data[[1]]
  rows_added <- 0
  for(i in 1:length(first_col)){
    if(!grepl("^\\s\\s", first_col[i])){

      data <- data %>%
        add_row(.before = i+rows_added)

      rows_added <- rows_added + 1
    }
  }


  data <- data %>%
    replace_labels(labels, add = TRUE)
  data
}

#returns a vector that represnts the column in which a group ends
get_groups <- function(data){

  separator = "\\^\\*\\^" # will probably be passed as para, in future
  group = c()

  labels <- data %>%
    get_labels() #gets label attribute for each column

  #gets second layer of headers by delimiter
  second_headers <- labels %>%
    sapply(function(x){
      if(grepl(separator,x)){
        strsplit(x,separator, fixed = FALSE)[[1]][1]
      }
      else
        ""
    })

  prev_header <- second_headers[1]
  for(i in 2:length(labels)){
    if(second_headers[i] != prev_header || (prev_header == "" && second_headers[i] == "")){
      group <- group %>%
        append(i-1)

    }
    prev_header <- second_headers[i]
  }
  group
}




