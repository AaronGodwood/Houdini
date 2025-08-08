# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#import libraries
library(logger)
library(tidyverse)
library(haven)
library(officer)
library(flextable)
library(readxl)
library(tictoc)



#location for some tables
location <- "/DATA/projects/slk/hs/hs301/blinded/primary_dryrun/data/tfls/external/"
table_name <- "t_14_01_01_01_t_disp.sas7bdat"

# Set up location of SAS datasets
location <- "/DATA/projects/slk/hs/hs301/blinded/dsmb_02/data/tfls/external/"


# Read in word document
Input_Doc <- read_docx("Houdini test with DSMB outputs.docx")
Input_Excel <- read_excel("Houdini DSMB Bookmark codes.xlsx")

# Set up word document formats
word_size <- docx_dim(Input_Doc)
width <- word_size$page['width'] - word_size$margins['left'] - word_size$margins['right']
border_style = officer::fp_border(color="black", width=1)





apply_flextable_defaults <- function(ft) {
  ft <- ft %>%
    fontsize(size = 10) %>%             # Set font size to 10
    fontsize(size = 10, part = "header") %>%
    font(font = "Times New Roman") %>%   # Set font to Times New Roman
    font(font = "Times New Roman", part = "header") %>%   # Set font to Times New Roman
    bold(part = "header") %>%  # set header to bold
    padding(padding = 0) %>%            # Set padding to 0
    line_spacing(space = 1) %>%          # Set line spacing to 1
    set_table_properties(layout = "autofit")  # Autofit the table layout
  return(ft)
}




# Set up function to add table
add_table <- function(x, bookmark, value){
  tic("cursor")
  x <- x %>%
    cursor_to_bookmark(bm_jmptbl,bookmark)
  #x <- cursor_bookmarks(x, bookmark)
  #x <- cursor_begin(x)
  toc()
  tic("table")
  x <-  body_add_flextable(x = x, value = value, align = "center", pos = "after")
  toc()
  x
}

#replaces old labels with new ones
replace_labels <- function(data, new_labels, add = FALSE){ #can maybe make this fucntional - faster/less memory
  #tic("label changes")
  labels <- data %>%
    lapply(function(x) attr(x, "label"))

  for(i in seq_along(new_labels)){
    if(!is.null(labels[[i]]) | add == TRUE)
      attr(data[[i]], "label") <- new_labels[[i]]
    else
      attr(data[[i]], "label") <- ""
  }
  #toc()
  data
}



update_word <- function(input_doc, input_sheet){
  #temporary assignments for testing
  input_doc <- Input_Doc
  input_sheet <- Input_Excel



  new_doc <- input_doc

  bm_jmptbl <- find_all_bookmarks(input_doc)

  #calculate number of standard format tables
  n_tables <- input_sheet %>%
    dim()

  #gets tables of standard formats - testing purposes
  standard_tables <- input_sheet %>%
    filter(Format == "Standard")
  n_standard <- standard_tables %>%
    dim()
  std_bookmarks <- standard_tables$Bookmark
  std_orientations <- standard_tables$Landscape %>%
    replace(is.na(.),FALSE)
  std_names <- standard_tables$Dataset




  #gets bookmarks of all tables
  bookmarks <- input_sheet$Bookmark
  orientations <- input_sheet$Landscape %>%
    replace(is.na(.),FALSE)

  #gets dataset names of all tables
  dataset_names <- input_sheet$Dataset


  for(i in 1:n_standard[1]){
    tic(str_glue("Table {i}"))

    new_doc <- new_doc %>%
      add_table(std_bookmarks[i], prep_table(std_names[i],std_orientations[i]))

    #print(prep_table(std_names[i],std_orientations[i]))
    log_info("Table {i}: {std_names[i]} inserted at bookmark: {std_bookmarks[i]}", namespace = "Houdini Logs")
    toc()
  }

  new_doc
}


prep_table <- function(table_name, landscape = FALSE){

  #pull raw data from .sas7bdat file
  raw_data <- str_c(location, table_name, "") %>%
    read_sas() %>%
    dplyr::select(-starts_with(c("ROWORD", "PAGE"))) %>%
    mutate(across(where(is.character), ~ gsub("\\|n", "\n", .))) # replaces |n with \n in data columns


  new_labels <- raw_data %>%
    lapply(function(x) attr(x, "label")) %>% #gets label attribute for each column
    lapply(function(x) gsub("\\|n", "\n", x)) #replaces |n with \n in each label

  #replaces old labels with new cleaned ones
  raw_data <- raw_data %>%
    replace_labels(new_labels)


  #gets descriptor columns
  chr_cols <- raw_data %>%
    select(where( ~ any(grepl("[A-Za-z]", .)))) %>%
    names()

  if(landscape == TRUE){
    raw_data <- raw_data %>%
      transpose_data()
  }


  # Format table - left aligns descriptor columns - this whole section can be cleaned up nicely later
  sas_data <- raw_data %>%
    #mutate(across(everything(), ~ gsub("\\|n\\b", "\n", .x))) %>%
    relocate(ROWLBL1) %>%
    dplyr::select(-starts_with(c("ROWORD", "PAGE"))) %>%
    flextable() %>%
    align(align = c("center"), part = "all") %>%  #align all columns (to be just data columns) centrally
    align(align = c("left"), part = "all", j = chr_cols) %>% #aligns descriptor columns to the left
    apply_flextable_defaults() #applies default formatting - times new roman(10), bold header, etc.

  if(landscape == TRUE){
    chr_rows <- which(names(new_labels) %in% chr_cols) - 1

    sas_data <- sas_data %>%
      align(align = c("center"), part = "all") %>%  #align all columns (to be just data columns) centrally
      align(align = c("left"), part = "body", i = chr_rows[chr_rows != 0]) %>% #aligns descriptor rows to the left
      transpose_flextable()
  }

  #returns sas_data
  sas_data
}


log_threshold(DEBUG)
log_info("Script start", namespace = "Houdini Logs")

tic("Start-Finish")
output_doc <- update_word(Input_Doc,Input_Excel)
toc()
print(tic.log(format = TRUE))


print(output_doc, target="Houdini_Test_1.docx")
