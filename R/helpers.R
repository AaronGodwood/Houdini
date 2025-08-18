

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
    strrep(" ", .)

  merge_columns(indents,column)
}

fill_gaps <- function(data){
  unique_data <- data[data != ""] %>%
    unique()

  data <- rep(unique_data, length.out = length(data))
}


#returns a vector that represents the column in which a group ends
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

get_groups_rows <- function(data){


  groups <- data %>%
    sapply( function(col){
      group <- c()
      prev_cell <- col[1]
      prev_indent <- grepl("^\\s", prev_cell)
      for(i in 2:length(col)){
        if((grepl("^\\s", col[i]) != prev_indent) || (prev_cell == "" && col[i] != ""))
        {
          if(!(i %in% groups)){
            group <- group %>%
              append(i)
          }
        }
        prev_cell <- col[i]
        prev_indent <- grepl("^\\s", prev_cell)
      }
      group
    })
  groups <- groups %>%
    flatten() %>%
    unique() #%>%
  #replace(which(groups == 2),1)
}
