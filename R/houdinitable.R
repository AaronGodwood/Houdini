

houdinitable <- function(data, col_keys = names(data),
                         cwidth = 0.75, cheight = 0.25,
                         defaults = list(),
                         use_labels = TRUE){
  stopifnot(is.data.frame(data),ncol(data) > 0)
  if(any(duplicated(col_keys))){
    stop("Can't have duplicated col keys")
  }
  list_lbls <- data %>%
    get_labels()

  invis_cols_names <- setdiff(names(data), col_keys)
  if(length(invis_cols_names) > 0){
    invis_cols <- lapply(invis_cols_names, function(x, n) character(n), nrow(data))
    invis_cols <- invis_cols %>%
      setNames(invis_cols_names)
    data[invis_cols_names] <- invis_cols
  }

  body <- houdini_tabpart(data = data, col_keys = col_keys, cwidth = cwidth, cheight = cheight)

  header_data <- data %>%
    get_labels() %>%
    split_headers()
  header_data <- data.frame(header_data)


  header <- houdini_tabpart(data = header_data, col_keys = col_keys, cwidth = cwidth, cheight = cheight)
  spans <- data[FALSE, , drop = FALSE]
  for(i in 1:nrow(header_data)){
    spans[i,] = get_runs(header_data[i,])
  }
  header$spans <- spans
  footer_data <- header_data[FALSE, , drop = FALSE]
  footer <- houdini_tabpart(data = footer_data, col_keys = col_keys, cwidth = cwidth, cheight = cheight)

  opts_word <- list(
    split = TRUE,
    keep_with_next = FALSE
  )

  properties <- list(
    layout = "autofit",
    width = 1,
    align = "center",
    opts_word = opts_word
  )

  out <- list(
    header = header,
    body = body,
    footer = footer,
    col_keys = col_keys,
    invis_cols_names = invis_cols_names,
    properties = properties
  )

  class(out) <- c("houdinitable")

  #out <- set_table_properties

  #apply_labels(out, labels)
  out

}


split_headers <- function(headers){
  delimiter <- houdini_global$defaults$delimiter
  headers <- headers %>%
    sapply(function(x){
      headers <- strsplit(x,delimiter, fixed = FALSE)

  })
  headers <- headers %>%
    sapply(function(x){
      if(length(x) != max(lengths(headers))){
        x <- c(rep("",(max(lengths(headers))-length(x))),x)

      }
      x
    }) %>%
    rbind()

}


get_runs <- function(list){
  if(purrr::is_empty(list))
  {
    return(c(0))
  }
  else if(length(list) == 1){
    return(c(1))
  }
  runs <- integer(length(list))
  count <- 1
  count_index <- 1
  prev_elem <- list[1]
  for(i in 2:length(list)){
    if(list[i] == prev_elem)
    {
      count <- count + 1
      runs[i] = 0

    }
    else
    {
      runs[count_index] = count
      count_index = i
      count = 1
    }
    prev_elem = list[i]
  }
  runs[count_index] = count
  runs
}

add_footer <- function(ht,footers){
  ht$footer$dataset <- ht$footer$dataset %>%
    sapply(function(x){
      c(x,footers)
    }) %>%
    data.frame()
  spans <- data[FALSE, , drop = FALSE]
  for(i in 1:nrow(ht$footer$dataset)){
    spans[i,] = get_runs(ht$footer$dataset[i,])
  }
  ht$footer$spans <- spans
  ht
}
