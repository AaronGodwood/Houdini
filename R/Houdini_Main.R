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
library(xml2)

library(readxl)
library(tictoc)


#testing purposes
display_tables <- function(names, directory){
  tic("Processing 115 tables")
  for(i in 1:length(names))
  {
    tryCatch(
      {
        tic(str_glue("Table{i}"))
        #returns updated doc with table added
        print(prep_table(names[i]))

        #logs a success if table is added correctly
        log_info("Table {i}:", namespace = "Houdini Logs")
        toc()

      },
      error = function(e){
        #logs a failure containing the error that occurred
        log_error("Table {i}: Error: {e}", namespace = "Houdini Logs")
        toc()
        #returns unchanged document
      }
    )
  }
  toc()


}


prep_rtf <- function(table_name, file_location,hide_data = FALSE){
  raw_rtf <- sprintf("%s%s",file_location,table_name) %>%
    read_raw_rtf()
  xml_table <- build_table(raw_rtf,hide_data)
  xml_table
}


#' Reads in raw data from .sas7bdat file and converts it to a formatted table
#'
#' @param table_name a string representing the file name of a dataset
#' @param format a string that says weather the dataset is in standard format or not
#' @param landscape a Boolean that says weather a table should be landscape <might get removed>
#' @param header_code a header code for adding headers missing from data sets
#' @param doc_width a float that represents the total width the table needs to be
#'
#' @return a formatted table
#' @export
#'
#' @examples
prep_table <- function(table_name, file_location, filters = "", footnotes = "", header_code = NULL, doc_width = 6.5){


  #pull raw data from .sas7bdat file and apply some cleaning - remove extraneous columns, format escape characters correctly
  raw_data <- sprintf("%s%s",file_location,table_name) %>%
    haven::read_sas() %>%
    #dplyr::select(-starts_with(c("ROWORD","PAGE"))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\\|n", "\n", .))) # replaces |n with \n in data columns



  #gets new, well formatted labels
  new_labels <- raw_data %>%
    get_labels %>% #gets label attribute for each column
    lapply(function(x) gsub("\\|n", "\n", x)) #replaces |n with \n in each label

  #replaces old labels with new cleaned ones
  raw_data <- raw_data %>%
    replace_labels(new_labels)



  #will be removed post-testing
  if(is.null(raw_data$COL1))
  {

    raw_data <- raw_data %>%
      non_standard_format()
  }
  # if(!is.null(raw_data$TRTLBL12)){
  #   needs_headers <- FALSE
  # }
  # if(needs_headers){
  #
  #   data <-raw_data %>%
  #     dplyr::select(starts_with(c( houdini_global$defaults$cols.name, houdini_global$defaults$rowlbls.name )))
  #   print(paste0(table_name,": " ,ncol(data)))
  # }


  if(nrow(raw_data) <= 1){
    logger::log_warn("{table_name} has no data", namespace = "Houdini Logs")
  }



  #Applies default formatting - times new roman(10), bold header, etc.
  ht <- raw_data %>%
    standard_format(header_code = header_code, doc_width = doc_width, filters = filters) %>%
    add_footnote(footnotes)

  #returns sas_data
  ht
}

#' @title Performs full Houdini operation
#'
#'Inserts all tables specified in the indicated
#'
#'
#'
#'
#' @param input_doc a string stating the file name for a word document populated with bookmarks fro table insertion
#' @param input_sheet a string stating the file name for a an excel sheet with information about what tables to insert and where to insert them
#' @param file_location a string representing the file path that the data sets reside in
#'
#' @return Outputs a word document with tables added
#' @export
#'
#' @examples
apparate <- function(input_doc,input_sheet,file_location, hide_data = FALSE, rtf = FALSE){

  #testing
  tic("Start-Finish")
  #sets up log file and log level
  setup_log()
  if(hide_data)
    log_info("Data is being hidden", namespace = "Houdini Logs")
  START <- structure(400L, level = "START/END", class = c("loglevel", "integer"))
  log_level(START,"Script Start:", namespace = "Houdini Logs")
  log_info("Directory: {file_location}", namespace = "Houdini Logs")
  log_info("User: {Sys.info()[[\"user\"]]}", namespace = "Houdini Logs")
  #reads in word document with bookmarks & logs it, stops operation if this fails as the program cannot continue without the word doc
  tic("Load Info:")
  tryCatch(
    {
      log_info("Sucessfully read document: {input_doc}", namespace = "Houdini Logs")
      Input_Doc <- read_docx(input_doc)
    },
    error = function(e){
      log_error("Error in reading {input_doc}: {e}", namespace = "Houdini Logs")
      stop("Error in reading Word document: refer to log")
    }
  )
  #reads in excel document containing instructions & logs it
  tryCatch(
    {
      log_info("Sucessfully read document: {input_sheet}", namespace = "Houdini Logs")
      Input_Sheet <- readxl::read_excel(input_sheet, sheet = 1)
    },
    error = function(e){
      log_error("Error in reading {input_sheet}: {e}", namespace = "Houdini Logs")
      stop("Error in reading Excel document: refer to log")
    }
  )
  #gets document dimensions
  word_size <- docx_dim(Input_Doc)
  width <- word_size$page['width'] - word_size$margins['left'] - word_size$margins['right']
  #prepares new doc for changes + output
  new_doc <- Input_Doc
  #generates jumptable of all bookmarks in the word doc
  tryCatch(
    {
      bm_jmptbl <- find_all_bookmarks(Input_Doc)
    },
    error = function(e){
      log_error("Error in reading bookmarks: {e}", namespace = "Houdini Logs")
      stop("Error in reading bookamrks: refer to log")
    }
  )
  if(check_excel(Input_Sheet)){
    stop("Check required columns in excel doc: Dataset, Bookmark, Footnotes, Notes")
  }
  #calculate number of tables
  n_tables <- Input_Sheet %>%
    dim()
  #gets bookmarks of all tables
  bookmarks <- Input_Sheet$Bookmark
  #gets dataset names of all tables
  dataset_names <- Input_Sheet$Dataset
  #gets footnotes of a table
  footnotes <- Input_Sheet$Footnotes %>%
    replace(is.na(.),"")
  #gets any notes about the table/figure
  filters <- Input_Sheet$Notes %>%
    sapply(function(x){
      sort_filters(x)
    })
  #gets header data - this functionality may not be needed later
  header_codes <- Input_Sheet$Header %>%
    replace(is.na(.), "") %>%
    sapply(function(x){
      strsplit(x,split = "")
      })
  toc()
  #adds each table to the word doc at its respective bookmark
  tic("Tables")

  for(i in 1:(n_tables[1])){



    # new_doc <- new_doc %>%
    #    add_houdinitable(bookmarks[i],prep_table(dataset_names[i],filters[i],footnotes[i],header_codes[[i]]))
    new_doc <- tryCatch(
      {
        if(is_table(bookmarks[i]))
        {
          tic(str_glue("Table {i}"))
          if(rtf){
            name <- dataset_names[i] %>%
              gsub("\\.sas7bdat","\\.rtf",.)
            test <- prep_rtf(name,file_location,hide_data)
            new_doc <- new_doc %>%
              add_xml_table(bookmark = bookmarks[i],test)
          }
          else{
            #returns updated doc with table added
            test <- prep_table(dataset_names[i],file_location = file_location,filters = filters[[i]],footnotes = footnotes[i], header_code = header_codes[[i]])
            #logs a success if table is added correctly
            new_doc <- new_doc %>%
              add_houdinitable(bookmarks[i],test,hide_data)
          }
          log_info("Table {i}: {dataset_names[i]} inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
          toc()
          new_doc
        }
        else
        {
          logger::log_level(TBLSTART,"Figure {i}: {dataset_names[i]}",namespace = "Houdini Logs")
          tic(str_glue("Figure {i}"))
          img_width <- (get_png_size(dataset_names[i])[["width"]])/96
          img_height <- (get_png_size(dataset_names[i])[["height"]])/96
          new_height <- img_height*(width/img_width)
          log_info("Figure {i}: {dataset_names[i]} inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
          toc()
          new_doc <- new_doc %>%
            add_figure(bookmarks[i],dataset_names[i],width = width, height = new_height)
        }
      },
      error = function(e){
        #logs a failure containing the error that occurred
        log_error("Table {i}: Error: {e} - {dataset_names[i]} was not inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
        toc()
        #returns unchanged document
        new_doc
      }
    )

  }
  toc()
  #stores final doc in new variable
  output_doc <- new_doc
  #outputs final document
  tic("Output final doc")
  output_docx(output_doc, target="Houdini_Test_1.docx")
  log_level(START,"Script Finish:", namespace = "Houdini Logs")
  toc()
  toc()
}

#sets up log output file and log level
#' Sets up log output file and log levels
#'
#' @return nothing
#' @export
#'
#' @examples
setup_log <- function()
{
  #sets up log output file
  logger::log_appender(appender_file("houdini.log"))
  #sets log level
  logger::log_threshold(DEBUG)
}







r <- function(){
  #location for some tables
  location2 <- "/DATA/projects/slk/hs/hs301/blinded/primary_dryrun/data/tfls/external/"
  table_name <- "t_14_01_02_01_t_demog.sas7bdat"
  table_names <- list.files(location2)
  table_names <- table_names[startsWith(table_names,"t")]


  # Set up location of SAS datasets
  location1 <- "/DATA/projects/slk/hs/hs301/blinded/dsmb_02/data/tfls/external/"

  location3 <- "/DATA/projects/slk/ppp/ppp201/unblinded/instream/data/tfls/internal/"

  location4 <- "/DATA/projects/slk/psa/psa301/blinded/instream/data/tfls/internal/"

  location5 <- "/DATA/projects/slk/hs/hs301/blinded/primary_dryrun/tfls/tables/external/"

  file_location <- location2
  location <- file_location

  # Read in word document
  # input_doc <- "Houdini test with DSMB outputs.docx"
  # input_sheet <- "Houdini DSMB Bookmark codes.xlsx"

  input_doc <- "M1095_HS_301_ClinicalStudyReport_Shell_V2_Draft2_Review_23June_responses_With bookmarks.docx"
  input_sheet <- "VELA-1 CSR Dry run test.xlsx"


  apparate(input_doc,input_sheet,file_location, rtf = FALSE, hide_data = TRUE)
}





