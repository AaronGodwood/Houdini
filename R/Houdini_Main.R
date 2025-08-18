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


#testing purposes
display_table <- function(name, directory){
  print(prep_table(name))
}









#superceeded by apparate() - kept for now for reference
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
    dplyr::filter(Format == "Standard")
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
          add_table(std_bookmarks[i], prep_table(std_names[i],std_orientations[i],header_codes),bm_jmptbl)

      },

      error = function(e){
        log_error("Table {i}: Error: {e} - {std_names[i]} was not inserted at bookmark: {std_bookmarks[i]}", namespace = "Houdini Logs")
        new_doc
      }
    )
    print(prep_table(std_names[i],FALSE))


  }

  # for(i in 1:length(table_names))
  # {
  #   print(prep_table(table_names[i],FALSE))
  # }

  new_doc
}



#' Reads in raw data from .sas7bdat file and converts it to a formatted table
#'
#' @param table_name a string representing the file name of a dataset
#' @param format a string that says weather the dataset is in standard format or not
#' @param landscape a Boolean that says weather a table should be landscape <might get removed>
#' @param header_code a header code for adding headers missing from data sets
#'
#' @return a formatted table
#' @export
#'
#' @examples
prep_table <- function(table_name, format , landscape = FALSE, header_code = NULL){

  #pull raw data from .sas7bdat file and apply some cleaning - remove extraneous columns, format escape characters correctly
  raw_data <- stringr::str_c(location, table_name, "") %>%
    haven::read_sas() %>%
    dplyr::select(-starts_with(c("ROWORD", "PAGE"))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\\|n", "\n", .))) # replaces |n with \n in data columns

  #gets new, well formatted labels
  new_labels <- raw_data %>%
    get_labels %>% #gets label attribute for each column
    lapply(function(x) gsub("\\|n", "\n", x)) #replaces |n with \n in each label

  #replaces old labels with new cleaned ones
  raw_data <- raw_data %>%
    replace_labels(new_labels)

  #makes sure the descriptor row is first if it exists
  if(!is.null(raw_data$ROWLBL1))
  {
    raw_data <- raw_data %>%
      dplyr::relocate(ROWLBL1)
  }

  if(format != "Standard"){
    raw_data <- raw_data %>%
      non_standard_format()
  }

  #same as transpose bit below
  # if(landscape == TRUE){
  #   raw_data <- raw_data %>%
  #     transpose_data()
  # }

  #Applies default formatting - times new roman(10), bold header, etc.
  ft <- raw_data %>%
    standard_format(header_code)

  #This doesn't really work right now, leaving it in to come back to it
  # if(landscape == TRUE){
  #   chr_rows <- which(names(new_labels) %in% chr_cols) - 1
  #   sas_data <- sas_data %>%
  #     align(align = c("center"), part = "all") %>%  #align all columns (to be just data columns) centrally
  #     align(align = c("left"), part = "body", i = chr_rows[chr_rows != 0]) %>% #aligns descriptor rows to the left
  #     transpose_flextable()
  # }

  #returns sas_data
  ft
}

#' Performs full Houdini operation
#'
#' @param input_doc a string stating the file name for a word document populated with bookmarks fro table insertion
#' @param input_sheet a string stating the file name for a an excel sheet with information about what tables to insert and where to insert them
#' @param file_location a string representing the file path that the data sets reside in
#'
#' @return
#' @export
#'
#' @examples
apparate <- function(input_doc,input_sheet,file_location){
  #testing
  tic("Start-Finish")
  #sets up log file and log level
  setup_log()
  START <- structure(400L, level = "START/END", class = c("loglevel", "integer"))
  log_level(START,"Script Start:", namespace = "Houdini Logs")
  log_info("Directory: {file_location}", namespace = "Houdini Logs")
  log_info("User: {Sys.info()[[\"user\"]]}", namespace = "Houdini Logs")
  #reads in word document with bookmarks
  tic("Load Info:")
  tryCatch(
    {
      log_info("Sucessfully read document: {input_doc}", namespace = "Houdini Logs")
      Input_Doc <- officer::read_docx(input_doc)
    },
    error = function(e){
      log_error("Error in reading {input_doc}: {e}", namespace = "Houdini Logs")
      stop("Error in reading Word document: refer to log")
    }
  )
  #reads in excel document containing instructions
  tryCatch(
    {
      log_info("Sucessfully read document: {input_sheet}", namespace = "Houdini Logs")
      Input_Sheet <- readxl::read_excel(input_sheet)
    },
    error = function(e){
      log_error("Error in reading {input_sheet}: {e}", namespace = "Houdini Logs")
      stop("Error in reading Excel document: refer to log")
    }
  )
  #prepares new doc for changes + output
  new_doc <- Input_Doc
  #generates jumptable of all bookmarks in the word doc
  bm_jmptbl <- find_all_bookmarks(Input_Doc)
  #calculate number of tables
  n_tables <- Input_Sheet %>%
    dim()
  #gets bookmarks of all tables
  bookmarks <- Input_Sheet$Bookmark
  #gets orientations of all tables
  orientations <- Input_Sheet$Landscape %>%
    replace(is.na(.),FALSE)
  #gets dataset names of all tables
  dataset_names <- Input_Sheet$Dataset
  #gets formats of tables
  formats <- Input_Sheet$Format
  #gets header data - this functionality may not be needed later
  header_codes <- Input_Sheet$Header %>%
    replace(is.na(.), "") %>%
    sapply(function(x){
      strsplit(x,split = "")
      })
  toc()
  #adds each table to the word doc at its respective bookmark
  for(i in 1:(n_tables[1])){

    new_doc <- tryCatch(
      {
        tic(str_glue("Table{i}"))
        #returns updated doc with table added
        test <- prep_table(dataset_names[i],formats[i],orientations[i],header_codes[[i]])
        print(test)
        #logs a success if table is added correctly
        log_info("Table {i}: {dataset_names[i]} inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
        toc()
        new_doc %>%
          add_table(bookmarks[i],test ,bm_jmptbl)

      },
      error = function(e){
        #logs a failure containing the error that occurred
        log_error("Table {i}: Error: {e} - {dataset_names[i]} was not inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
        #returns unchanged document
        new_doc
      }
    )
    #testing purposes
    #print(prep_table(dataset_names[i],orientations[i],header_codes[[i]]))
  }
  #stores final doc in new variable
  tic("possible")
  output_doc <- new_doc
  toc()
  #outputs final document
  tic("Output final doc")
  print(output_doc, target="Houdini_Test_1.docx")
  log_level(START,"Script Finish:", namespace = "Houdini Logs")
  toc()
  toc()
}

#sets up log output file and log level
#' Sets up log output file and log levels
#'
#' @return
#' @export
#'
#' @examples
setup_log <- function()
{
  #sets up log output file
  log_appender(appender_file("Houdini_log.log"))
  #sets log level
  log_threshold(DEBUG)
}




#location for some tables
location <- "/DATA/projects/slk/hs/hs301/blinded/primary_dryrun/data/tfls/external/"
table_name <- "t_14_03_01_01_t_teae_ovrl.sas7bdat"
table_names <- list.files(location)

# Set up location of SAS datasets
location <- "/DATA/projects/slk/hs/hs301/blinded/dsmb_02/data/tfls/external/"


# Read in word document
input_doc <- "Houdini test with DSMB outputs.docx"
input_sheet <- "Houdini DSMB Bookmark codes.xlsx"

#word_size <- docx_dim(Input_Doc)
#width <- word_size$page['width'] - word_size$margins['left'] - word_size$margins['right']
#border_style = officer::fp_border(color="black", width=1)


apparate(input_doc,input_sheet,location)




