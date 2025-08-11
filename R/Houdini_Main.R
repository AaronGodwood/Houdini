#
#
#
#
#
#

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
table_name <- "t_14_01_02_01_t_demog.sas7bdat"

# Set up location of SAS datasets
location <- "/DATA/projects/slk/hs/hs301/blinded/dsmb_02/data/tfls/external/"


# Read in word document
Input_Doc <- read_docx("Houdini test with DSMB outputs.docx")
Input_Excel <- read_excel("Houdini DSMB Bookmark codes.xlsx")

# Set up word document formats
word_size <- docx_dim(Input_Doc)
width <- word_size$page['width'] - word_size$margins['left'] - word_size$margins['right']
border_style = officer::fp_border(color="black", width=1)

#testing purposes
display_table <- function(name, directory){
  print(prep_table(name))
}





# Set up function to add table
add_table <- function(x, bookmark, value,jmp_tbl){
  x <- x %>%
    cursor_to_bookmark(jmp_tbl,bookmark)
  #x <- cursor_bookmark(x, bookmark)
  #x <- cursor_begin(x)
  x <-  body_add_flextable(x = x, value = value, align = "center", pos = "on")
  x
}





update_word <- function(input_doc, input_sheet){
  #temporary assignments for testing
  input_doc <- Input_Doc
  input_sheet <- Input_Excel



  new_doc <- input_doc

  #generates jumptable of all bookmarks
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

    new_doc <- tryCatch(
      {
        log_info("Table {i}: {std_names[i]} inserted at bookmark: {std_bookmarks[i]}", namespace = "Houdini Logs")
        new_doc %>%
          add_table(std_bookmarks[i], prep_table(std_names[i],std_orientations[i]),bm_jmptbl)

      },

      error = function(e){
        log_error("Table {i}: Error: {e} - {std_names[i]} was not inserted at bookmark: {std_bookmarks[i]}", namespace = "Houdini Logs")
        new_doc
      }
    )


    #print(prep_table(std_names[i],std_orientations[i]))


  }

  new_doc
}

raw_data_processing <- function(raw_data){
  raw_data <- raw_data %>%
    remove_dupes("")
}

remove_dupes <- function(col,prev_value){
  if(is_empty(col)){
    c()
  }
  if(col[1] == prev_value){
    c("",remove_dupes(col[-1],prev_value))
  }
  else{
    prev_value <- col[1]
    c(col[1],remove_dupes(col[-1],prev_value))
  }
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
    add_buffers() %>%
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

log_appender(appender_file("Houdini_log.log"))
log_threshold(DEBUG)
username <- Sys.info()[["user"]]
START <- structure(400L, level = "START/END", class = c("loglevel", "integer"))
log_level(START,"Script Start:", namespace = "Houdini Logs")
log_info("Directory: {location}", namespace = "Houdini Logs")
log_info("User: {username}", namespace = "Houdini Logs")

tic("Start-Finish")
output_doc <- update_word(Input_Doc,Input_Excel)
toc()



print(output_doc, target="Houdini_Test_1.docx")
log_level(START,"Script Finish:", namespace = "Houdini Logs")
