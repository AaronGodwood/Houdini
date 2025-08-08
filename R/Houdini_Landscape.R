transpose_flextable <- function(ft){
  ft <- ft %>%
    border_remove() %>%
    vline(j = c(1), border = fp_border_default()) %>%
    vline_left(border = fp_border_default(), part = "all") %>%
    bold(part = "body", j = c(1))


  return(ft)
}

transpose_data <- function(data){

  new_labels <- data %>%
    lapply(function(x) attr(x, "label")) %>% #gets label attribute for each column
    lapply(function(x) gsub("\\|n", "\n", x)) #replaces |n with \n in each label

  labels <- new_labels %>%
    names()

  #gets labels from first column
  new_labels <- data[,1] %>%
    t() %>%
    as.data.frame()

  #transposes all but first column then adds previews first column as header
  data <- data[-1] %>%
    t() %>%
    as.data.frame() %>%
    replace_labels(new_labels, add = TRUE)

  data <- data %>%
    mutate(!!labels[1] := labels[-1], .before = 1)


  data #return transposed data

}
