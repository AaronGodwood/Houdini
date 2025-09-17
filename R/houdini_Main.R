






prep_rtf <- function(table_name, file_location,filters,hide_data = FALSE){
  raw_rtf <- sprintf("%s%s",file_location,table_name) %>%
    read_raw_rtf()
  xml_table <- build_table(raw_rtf,filters,hide_data)
  xml_table
}


#' Reads in raw data from .sas7bdat file and converts it to a formatted table
#'
#' @param table_name a string representing the file name of a dataset
#' @param file_location the file path to the dataset to be read
#' @param filters any filters to be performed on the table e.g. on;y Week 16 data
#' @param footnotes any footnotes to be added to the table
#' @param header_code a header code for adding headers missing from data sets
#' @param doc_width a float that represents the total width the table needs to be
#'
#' @return a formatted table
#' @importFrom haven read_sas
#' @importFrom dplyr mutate across where
#' @importFrom logger log_warn
#' @keywords internal
#'
prep_table <- function(table_name, file_location,  filters = "", footnotes = "", header_code = NULL, doc_width = 6.5){


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
#' @description Inserts all tables specified in the indicated
#'
#'
#'
#'
#' @param input_doc a string stating the file name for a word document populated with bookmarks fro table insertion
#' @param input_sheet a string stating the file name for a an excel sheet with information about what tables to insert and where to insert them
#' @param file_location a string representing the file path that the data sets reside in
#' @param hide_data A Boolean describing if the fucntion will hide data on output by replacing data with "XX"
#' @param rtf a Boolean describing if the functions extracts data from rtf files or not, defaults to FALSE
#'
#' @return Outputs a word document with tables added
#' @import logger
#' @importFrom readxl read_excel
#' @export
#'
apparate <- function(input_doc,input_sheet,file_location, figure_location = "", hide_data = FALSE, rtf = FALSE){

  #testing
  #tic("Start-Finish")
  #sets up log file and log level
  setup_log()
  if(hide_data)
    logger::log_info("Data is being hidden", namespace = "Houdini Logs")
  START <- structure(400L, level = "START/END", class = c("loglevel", "integer"))
  logger::log_level(START,"Script Start:", namespace = "Houdini Logs")
  logger::log_info("Directory: {file_location}", namespace = "Houdini Logs")
  logger::log_info("User: {Sys.info()[[\"user\"]]}", namespace = "Houdini Logs")
  #reads in word document with bookmarks & logs it, stops operation if this fails as the program cannot continue without the word doc
  #tic("Load Info:")
  tryCatch(
    {
      logger::log_info("Sucessfully read document: {input_doc}", namespace = "Houdini Logs")
      Input_Doc <- read_docx(input_doc)
    },
    error = function(e){
      logger::log_error("Error in reading {input_doc}: {e}", namespace = "Houdini Logs")
      stop("Error in reading Word document: refer to log")
    }
  )
  #reads in excel document containing instructions & logs it
  tryCatch(
    {
      logger::log_info("Sucessfully read document: {input_sheet}", namespace = "Houdini Logs")
      Input_Sheet <- readxl::read_excel(input_sheet, sheet = 1)
    },
    error = function(e){
      logger::log_error("Error in reading {input_sheet}: {e}", namespace = "Houdini Logs")
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
      logger::log_error("Error in reading bookmarks: {e}", namespace = "Houdini Logs")
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
  #gets any filters for the table/figure
  timelines <- Input_Sheet$Timepoints %>%
    replace(is.na(.),"")
  parameters <- Input_Sheet$Parameters %>%
    replace(is.na(.),"")
  filters <- sort_filters(timelines,parameters)
  #gets header data - this functionality may not be needed later
  header_codes <- Input_Sheet$Header %>%
    replace(is.na(.), "") %>%
    sapply(function(x){
      strsplit(x,split = "")
      })
  codes <- Input_Sheet$Code[!is.na(Input_Sheet$Code)] %>%
    sort_codes()
  #toc()
  #adds each table to the word doc at its respective bookmark
  #tic("Tables")

  for(i in 1:(n_tables[1])){



    # new_doc <- new_doc %>%
    #    add_houdinitable(bookmarks[i],prep_table(dataset_names[i],filters[i],footnotes[i],header_codes[[i]]))
    new_doc <- tryCatch(
      {
        if(is_table(bookmarks[i]))
        {
          #tic(str_glue("Table {i}"))
          if(rtf){
            name <- dataset_names[i] %>%
              gsub("\\.sas7bdat","\\.rtf",.)
            test <- prep_rtf(name,file_location,filters[[i]],hide_data)
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
          logger::log_info("Table {i}: {dataset_names[i]} inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
          #toc()
          new_doc
        }
        else
        {
          #logger::log_level(TBLSTART,"Figure {i}: {dataset_names[i]}",namespace = "Houdini Logs")
          #tic(str_glue("Figure {i}"))
          if(figure_location != ""){
            figure_name <- paste0(figure_location,"/",dataset_names[i])
          }else{
            figure_name <- dataset_names[i]
          }
          img_width <- (get_png_size(dataset_names[i])[["width"]])/96
          img_height <- (get_png_size(dataset_names[i])[["height"]])/96
          new_height <- img_height*(width/img_width)

          #toc()
          new_doc <- new_doc %>%
            add_figure(bookmarks[i],figure_name,width = width, height = new_height)
          logger::log_info("Figure {i}: {dataset_names[i]} inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
          new_doc
        }
      },
      error = function(e){
        #logs a failure containing the error that occurred
        logger::log_error("Table {i}: Error: {e} - {dataset_names[i]} was not inserted at bookmark: {bookmarks[i]}", namespace = "Houdini Logs")
        #toc()
        #returns unchanged document
        new_doc
      }
    )

  }
  #toc()
  #stores final doc in new variable
  output_doc <- new_doc
  #outputs final document
  #tic("Output final doc")
  output_docx(output_doc, target="Houdini_Test_1.docx")
  logger::log_level(START,"Script Finish:", namespace = "Houdini Logs")
  #toc()
  #toc()
}

#sets up log output file and log level
#' Sets up log output file and log levels
#'
#' @return nothing
#' @import logger
#' @export
#'
setup_log <- function()
{
  file_name <- paste0("houdini[",Sys.time(),"].log")
  #sets up log output file
  logger::log_appender(appender_file(file_name))
  #sets log level
  logger::log_threshold(DEBUG)
}







r <- function(){
  #location for some tables
  location2 <- "/DATA/projects/slk/hs/hs301/blinded/primary_dryrun/data/tfls/external/"
  table_name <- "t_14_03_01_08_01_t_aesi_cat_pt.sas7bdat"

  table_names <- list.files(location2)
  table_names <- table_names[startsWith(table_names,"t")]


  # Set up location of SAS datasets
  location1 <- "/DATA/projects/slk/hs/hs301/blinded/dsmb_02/data/tfls/external/"

  location3 <- "/DATA/projects/slk/ppp/ppp201/unblinded/instream/data/tfls/internal/"

  location4 <- "/DATA/projects/slk/psa/psa301/blinded/instream/data/tfls/internal/"

  location5 <- "/DATA/projects/slk/hs/hs301/blinded/primary_dryrun/tfls/tables/external/"

  file_location <- location5
  location <- file_location

  # Read in word document
  # input_doc <- "Houdini test with DSMB outputs.docx"
  # input_sheet <- "Houdini DSMB Bookmark codes.xlsx"

  input_doc <- "M1095_HS_301_ClinicalStudyReport_Shell_V2_Draft2_Review_23June_responses_With bookmarks.docx"
  #input_doc <- "Houdini_Test_1.docx"
  #input_doc <- "noimages.docx"
  input_sheet <- "VELA-1 CSR Dry run test.xlsx"


  apparate(input_doc,input_sheet,file_location,rtf = TRUE, hide_data = TRUE)
}





