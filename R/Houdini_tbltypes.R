#Here temporarily for testing
code <- c("0","A","A","B","B","0")
std_labels = c(A = "Part A (Placebo Controlled)", B = "Part B (Maintainence)")
header_border = fp_border(color = "black" , width = 1.5)


shift_table <- function(data){
  grouped_data <- data %>%
    group_by(BYGRP,BYGRPN,ROWGRP1,ROWGRP2)
  temp <- grouped_data %>%
    summarise(
      N = n()
    )
}

apply_second_header <- function(data,second_labels,code){
  labels <- data %>%
    lapply(function(x) attr(x, "label")) #gets label attribute for each column

  for(i in 1:length(labels)){
    if(code[i] != "0"){
      labels[i] <- sprintf("%s^*^%s",second_labels[[code[i]]],labels[i])
    }
  }

  data <- data %>%
    replace_labels(labels, add = FALSE)
}


standard_format <- function(data){

  separator <- "\\^\\*\\^"

  #gets descriptor columns
  chr_cols <- data %>%
    select(where( ~ any(grepl("[A-Za-z]", .)))) %>%
    names()

  #gets data columns
  data_cols <- data %>%
    names() %>%
    setdiff(chr_cols)


  data <- data %>%
    apply_second_header(std_labels,code) %>%
    add_buffers()




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

  first_headers <- labels %>%
    sapply(function(x){
      if(grepl(separator,x)){
        strsplit(x,separator, fixed = FALSE)[[1]][2]
      }
      else
        x
    })

  data <- data %>%
    replace_labels(first_headers)

  ft <- data %>%
    dplyr::select(-starts_with(c("ROWORD", "PAGE"))) %>%
    flextable() %>%
    apply_flextable_defaults() %>%
    lift_headers(separator = "\\^\\*\\^",second_headers) %>%
    align(align = c("center"), part = "all") %>%  #align all columns (to be just data columns) centrally
    align(align = c("left"), part = "all", j = chr_cols)  #aligns descriptor columns to the left
  ft

}

#brings secondary headers marked by the ^*^ delimiter up to a second row of headers
lift_headers <- function(ft,separator,second_headers){



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
    hline_bottom(part = "body" , border = header_border) %>%
    merge_h(part = "header")

  ft
}


