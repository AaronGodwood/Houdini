

split_semi <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, ";", fixed = TRUE)[[1L]])
}




#' Perpares an rtf table as an xml table
#'
#' @param table_name name of rtf table document
#' @param file_location file path to that document
#' @param filters any filters to filter the table by
#' @param hide_data weather you want to replace all data with XX
#'
#' @return a table in Word XML format
#' @keywords internal
#'
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



  pattern <- sprintf("%s1", houdini_global$defaults$cols.name)
  if(is.null(raw_data[[pattern]]))
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

  #returns houdinitable
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
#' @param figure_location a string representing the file path that the figures reside in
#' @param hide_data A Boolean describing if the function will hide data on output by replacing data with "XX"
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
      Input_Doc <- open_docx(input_doc)
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




  # Must have Bookmark and Table columns (case-insensitive)
  col_lower <- tolower(names(Input_Sheet))
  bm_col  <- which(col_lower == "bookmark")[1L]
  tbl_col <- which(col_lower == "dataset")[1L]
  if (is.na(bm_col) || is.na(tbl_col)) {
    logger::log_error(
      "Excel file must contain 'Bookmark' and 'Dataset' columns"
    )
    stop("Excel file must contain 'Bookmark' and 'Dataset' columns")
  }

  #gets footnotes of a table
  footnotes <- Input_Sheet$Footnotes %>%
    replace(is.na(.),"")



  #gets header data - this functionality may not be needed later
  #header_codes <- Input_Sheet$Header %>%
   # replace(is.na(.), "") %>%
  #  sapply(function(x){
  #    strsplit(x,split = "")
  #    })
  #codes <- Input_Sheet$Code[!is.na(Input_Sheet$Code)] %>%
  #  sort_codes()


  config_data <- data.frame(
    Bookmark  = as.character(Input_Sheet[[bm_col]]),
    Table = tools::file_path_sans_ext(as.character(Input_Sheet[[tbl_col]])),
    stringsAsFactors = FALSE
  )
  # Replace NA with empty string
  config_data$Bookmark[is.na(config_data$Bookmark)]   <- ""
  config_data$Table[is.na(config_data$Table)] <- ""

  # Parse optional filter columns into table_selections
  # Recognised column names (case-insensitive): parameters, timelines
  param_col  <- which(col_lower == "parameters")[1L]
  tline_col  <- which(col_lower == "timepoints")[1L]

  sels <- list()

  for (i in seq_len(nrow(config_data))) {
    tname <- config_data$Table[i]
    if (!nzchar(tname)) next

    params <- if (!is.na(param_col)) split_semi(Input_Sheet[[param_col]][i]) else character()
    tlines <- if (!is.na(tline_col)) split_semi(Input_Sheet[[tline_col]][i]) else character()

    sels[[as.character(i)]] <- list(
      excluded_cols        = NULL,
      excluded_rows        = NULL,
      excluded_header_rows = NULL,
      parameters           = if (length(params) > 0L) params else NULL,
      timelines            = if (length(tlines) > 0L) tlines else NULL
    )
  }

  #get possible rtf files from folder
  rtf_files <- list.files(file_location, pattern = "\\.rtf$", ignore.case = TRUE)
  tbl_names  <- tools::file_path_sans_ext(rtf_files)
  full_paths <- file.path(file_location, rtf_files)
  rtf_paths <- setNames(as.list(full_paths), tbl_names)

  process_document(input_doc,config_data,rtf_paths,sels,paste0(getwd(),"/",input_doc,"Houdini_Output.docx", collapse = ""))



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













