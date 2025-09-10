
#checks the input excel doc has the required columns
check_excel <- function(doc){
  required_cols <- c("Dataset","Bookmark","Footnotes","Notes")
  return(any(sapply(required_cols, function(x){!(x %in% names(doc))})))
}

#combines two columns of strings
merge_columns <- function(col1,col2,separator = ""){
  merged <- mapply(function(cell1,cell2){
    if(cell1 !="")
      sprintf("%s%s%s",cell1,separator,cell2)
    else
      cell2
  },col1,col2)
  names(merged) <- NULL

  merged

}

#indents a label column with its specified indents
indent <- function(column, indent_column){
  #creates a column of n spaces based on number in the indent col - NA goes to 0
  indents <- indent_column %>%
    replace(is.na(.), 0) %>%
    strrep("  ", .)

  merge_columns(indents,column)
}

#fills gaps of blank data in a column
fill_gaps <- function(data){
  unique_data <- data[data != ""] %>%
    smallest_pattern()

  data <- rep(unique_data, length.out = length(data))
}

#gets the smallest repeating pattern in a run of data
smallest_pattern <- function(col){
  n <- length(col)
  for(i in 1:(n/2)){
    if( n%% i == 0){
      pattern <- col[1:i]
      if(all(rep(pattern, n/i) == col)){
        return(pattern)
      }
    }
  }
  return(col)
}

#gets all column names from a df that match a specfic pattern

#'
#' @importFrom dplyr select
#' @keywords internal
get_col_names <- function(data, col_type){
  pattern <- sprintf("^%s[0-9]$",col_type)
  data <- data %>%
    dplyr::select(grep(pattern,names(.)))
  data
}

#checks if a name starts with the word table
is_table <- function(name){
  name %>%
    startsWith("Table")
}


#gets dimensions of .png file
get_png_size <- function(image_path){
  con <- file(image_path,"rb")
  on.exit(close(con))

  readBin(con, "raw", 8)
  chunk_length <- readBin(con, "integer", 1, size = 4, endian = "big")
  chunk_type <- readBin(con, "character",1,size = 4)

  if(chunk_type == "IHDR"){
    width <- readBin(con, "integer", 1, size =4, endian = "big")
    height <- readBin(con, "integer", 1, size =4, endian = "big")
    c(width = (width/256), height = (height/256))
  }
  else
  {
    stop("Not a png")
  }

}

#sorts filters from excel doc into a named list e.g. parameters = c("filter1","filter2") timelines = c("filter3","filter4")
#'@importFrom stringr str_remove
#'@keywords internal
sort_filters <- function(raw_filters){
  if(is.na(raw_filters))
  {
    return(c())
  }


  raw_filters <- raw_filters %>%
    strsplit("; ") %>%
    unlist()

  filters1 = character(0)
  filters2 = character(0)
  for( i in seq_along(raw_filters)){
    if(grepl("Parameters: ", raw_filters[i])){
      filters2 <- raw_filters[i] %>%
      str_remove("Parameters: ") %>%
        strsplit(", ")%>%
        unlist()
    }
    else
    {
      filters1 <- raw_filters[i] %>%
        str_remove("Timelines: ") %>%
        strsplit(", ") %>%
        unlist()

    }
  }
  out <- list(
    parameters = filters2,
    timelines = filters1
  )
  out
}



#returns a vector that represents the column in which a group ends
get_groups <- function(data){

  delimiter <- houdini_global$defaults$delimiter
  group = c()

  labels <- data %>%
    get_labels() #gets label attribute for each column

  #gets second layer of headers by delimiter
  second_headers <- labels %>%
    sapply(function(x){
      if(grepl(delimiter,x)){
        strsplit(x,delimiter, fixed = FALSE)[[1]][1]
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


