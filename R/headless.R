
split_semi <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, ";", fixed = TRUE)[[1L]])
}

split_ints <- function(x){
  v <- as.integer(split_semi(x))
  v <- v[!is.na(v)]
  if(length(v) > 0) v else NULL
}



parse_xl <- function(xl){
  col_lower <- tolower(names(xl))
  bm_col  <- which(col_lower == "bookmark")[1L]
  tbl_col <- which(col_lower == "dataset")[1L]
  if (is.na(bm_col) || is.na(tbl_col)) {
    stop("Excel file must contain 'Bookmark' and 'Dataset' columns")
  }

  config <- data.frame(
    Bookmark  = as.character(xl[[bm_col]]),
    Table = tools::file_path_sans_ext(as.character(xl[[tbl_col]])),
    stringsAsFactors = FALSE
  )
  # Replace NA with empty string
  config$Bookmark[is.na(config$Bookmark)]   <- ""
  config$Table[is.na(config$Table)] <- ""

  # Parse optional filter columns into table_selections
  # Recognised column names (case-insensitive): parameters, timepoints
  param_col  <- which(col_lower == "parameters")[1L]
  tline_col  <- which(col_lower == "timepoints")[1L]

  #experimental at this point

  erow_col <- which(col_lower == "excludedrows")[1L]
  ecol_col <- which(col_lower == "excludedcolumns")[1L]
  ehdr_col <- which(col_lower == "excludedheaderrows")[1L]

  col_val <- function(col, i) if(is.na(col)) NA_character_ else xl[[col]][i]

  sels <- list()

  for (i in seq_len(nrow(config))) {
    if (!nzchar(config$Table[i])) next

    params <- split_semi(col_val(param_col, i))
    tlines <- split_semi(col_val(tline_col, i))

    sels[[as.character(i)]] <- list(
      excluded_cols        = split_ints(col_val(ecol_col,i)),
      excluded_rows        = split_ints(col_val(erow_col,i)),
      excluded_header_rows = split_ints(col_val(ehdr_col,i)),
      parameters           = if (length(params) > 0L) params else NULL,
      timelines            = if (length(tlines) > 0L) tlines else NULL
    )

  }

  list(config = config, selections = sels)
}


read_xlsx <- function(path) {
  parse_xl(readxl::read_excel(path, sheet = 1))
}


apparate <- function(input_doc,input_sheet,file_location, figure_location = "", hide_data = FALSE, rtf = FALSE, quiet = FALSE){
  #Check documents exist
  if(!file.exists(input_doc)){
    stop("Word document not found:", input_doc)
  }
  if(!dir.exists(file_location)){
    stop("RTF folder not found:", file_location)
  }

  #read in documents and gets rtf filenames

  parsed <- read_xlsx(input_sheet)

  rtf_files <- list.files(file_location, pattern = "\\.rtf$", ignore.case = TRUE)
  rtf_paths <- setNames(
    as.list(file.path(file_location, rtf_files)),
    tools::file_path_sans_ext(rtf_files)
  )

  #validates excel file and puls out filters

  cfg <- parsed$config
  keep <- which(nzchar(trimws(cfg$Bookmark)) & nzchar(trimws(cfg$Table)))
  if(length(keep) == 0){
    stop("No complete Bookmark/Table rows in excel")
  }
  cfg <- cfg[keep, , drop = FALSE]
  sels <- setNames(
    lapply(keep, function(i) parsed$selections[[as.character(i)]]),
    as.character(seq_along(keep))
  )

  #progress function for console output
  cb <- if(quiet) NULL else{
    function(i, n, msg) message(sprintf("[%d/%d] %s", i, n, msg),)
  }

  #runs main process and logs results

  output_path <-  paste0(getwd(),"/",input_doc," Houdini_Output.docx", collapse = "")

  status <- process_document(input_doc, cfg, rtf_paths, sels, output_path,
                             progress_cb = cb)
  write_log(input_doc, input_sheet, cfg ,sels, status, file_location)

}


write_log <- function(input_doc, input_sheet = NULL,config_data,sels,status, file_location){
  df    <- config_data


  lines <- character()

  lines <- c(lines,
             "  Houdini Document Generation Log",
             paste0("Generated : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
             paste0("User      : ", Sys.info()[["user"]]),
             paste0("Word file : ", if (!is.null(input_doc)) input_doc else "(not set)"),
             paste0("Excel File :", if(!is.null(input_sheet)) input_sheet else "(not set)"),
             paste0("RTF folder: ", file_location %||% "(not set)"),
             ""
  )

  valid_rows <- which(nzchar(trimws(df$Bookmark)) & nzchar(trimws(df$Table)))

  if (length(valid_rows) == 0L) {
    lines <- c(lines, "(no table mappings defined)")
  } else {
    gen_status <- status

    fmt_vec <- function(x, none = "(all)") {
      if (is.null(x) || length(x) == 0L) none else paste(x, collapse = "; ")
    }

    for (i in valid_rows) {
      bm_val  <- trimws(df$Bookmark[i])
      tbl_val <- trimws(df$Table[i])
      sel     <- sels[[as.character(i)]] %||% list()
      err     <- gen_status[[as.character(i)]]$err
      warn    <- gen_status[[as.character(i)]]$warn

      if (!is.null(err)) {
        err_msg  <- if (inherits(err, "houdini_error")) conditionMessage(err) else as.character(err)
        err_hint <- if (inherits(err, "houdini_error")) err$hint else NULL
        lines <- c(lines,
                   paste0("Row       : ", i),
                   paste0("Bookmark  : ", bm_val),
                   paste0("Table     : ", tbl_val, ".rtf"),
                   paste0("Status    : ERROR - ", err_msg),
                   if (!is.null(err_hint)) paste0("Hint      : ", err_hint) else NULL,
                   ""
        )
      } else {
        lines <- c(lines,
                   paste0("Row       : ", i),
                   paste0("Bookmark  : ", bm_val),
                   paste0("Table     : ", tbl_val, ".rtf"),
                   paste0("Parameters: ", fmt_vec(sel$parameters)),
                   paste0("Timepoints: ", fmt_vec(sel$timelines))
        )

        if(!is.null(warn) && length(warn) > 0){
          for(i in seq_along(warn)){
            warn_msg  <- if (inherits(warn[[i]], "houdini_warning")) conditionMessage(warn[[i]]) else as.character(warn[[i]])
            warn_hint <- if (inherits(warn[[i]], "houdini_warning")) warn[[i]]$hint else NULL
            lines <- c(lines,
                       paste0("Status    : WARNING - ", warn_msg),
                       if (!is.null(warn_hint)) paste0("Hint      : ", warn_hint) else NULL)
          }
        }
        lines <- c(lines, "")
      }
    }
  }
  path <- paste0("houdini_log[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "].log")
  writeLines(lines, path)
}
