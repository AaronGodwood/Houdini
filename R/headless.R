
split_semi <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, ";", fixed = TRUE)[[1L]])
}

split_ints <- function(x){
  v <- as.integer(split_semi(as.character(x)))
  v <- v[!is.na(v)]
  if(length(v) > 0) v else NULL
}


#' Parse a excel config into config + selections
#'
#' Shared by the app's Excel import and [apparate()]. Requires Bookmark and
#' Table columns (case-insensitive); recognises optional Parameters,
#' Timelines, ExcludedColumns, ExcludedRows and ExcludedHeaderRows columns,
#' all semicolon-separated. Any .rtf extension on Table values is stripped.
#'
#' @param xl A data.frame (e.g. from readxl) with the columns above
#' @return list(config = data.frame(Bookmark, Table), selections = list keyed
#'   by row index as character, matching the app's table_selections format)
#' @export
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
  # Recognised column names (case-insensitive): parameters, timepoints, levels
  param_col  <- which(col_lower == "parameters")[1L]
  tline_col  <- which(col_lower == "timepoints")[1L]
  lvl_col    <- which(col_lower == "levels")[1L]

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
    lvls   <- split_semi(col_val(lvl_col, i))

    sels[[as.character(i)]] <- list(
      excluded_cols        = split_ints(col_val(ecol_col,i)),
      excluded_rows        = split_ints(col_val(erow_col,i)),
      excluded_header_rows = split_ints(col_val(ehdr_col,i)),
      parameters           = if (length(params) > 0L) params else NULL,
      timelines            = if (length(tlines) > 0L) tlines else NULL,
      levels               = if (length(lvls)   > 0L) lvls   else NULL
    )

  }

  list(config = config, selections = sels)
}

#' Read a houdini xlsx workbook
#'
#' Same format as the app's Excel export: see [parse_xl()].
#'
#' @param path Path to the .xlsx file
#' @return list(config, selections); see [parse_xl()]
#' @export
read_xlsx <- function(path) {
  parse_xl(readxl::read_excel(path, sheet = 1))
}

#' Run the full houdini injection pipeline
#'
#' Injects every configured RTF table/image into the Word document at its
#' bookmark. Suitable for scripts and CI. Re-running on an already-generated
#' document replaces the previously injected content instead of duplicating it.
#'
#' @param input_doc Path to the template (or previously generated) .docx
#' @param input_sheet Path to a config .xlsx (see [read_xlsx()]) or a
#'   data.frame with Bookmark/Table columns plus optional filter columns
#' @param file_location Folder containing the table .rtf files named in the config
#' @param figure_location Folder containing the figure .rtf files named in the config (defaults to table location)
#' @param hide_data Option to replace all data with XX on insertion
#' @param rtf legacy option (kept for existing programs)
#' @param quiet Suppress progress and summary messages
#' @return Invisibly, the per-row status list: NULL for success or a
#'   houdini_error condition per failed row, keyed by config row
#' @export
apparate <- function(input_doc,input_sheet,file_location, figure_location = NULL, hide_data = FALSE, rtf = FALSE, quiet = FALSE){
  #Check documents exist
  if(!file.exists(input_doc)){
    stop("Word document not found: ", input_doc)
  }
  if(!dir.exists(file_location)){
    stop("RTF folder not found: ", file_location)
  }
  if(is.null(figure_location) || !dir.exists(figure_location)){
    figure_location <- file_location
  }

  #read in documents and gets rtf filenames

  parsed <- if(is.character(input_sheet)){
    read_xlsx(input_sheet)
  } else{
    parse_xl(input_sheet)
  }

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
    function(i, n, msg) message(sprintf("[%d/%d] %s", i, n, msg))
  }

  #runs main process and logs results

  output_path <-  if(!grepl("^/", input_doc)){
    paste0(getwd(),"/",input_doc,"_Houdini_Output.docx", collapse = "")
  } else{
    paste0(input_doc, "_Houdini_Output.docx", collapse = "")
  }

  status <- process_document(input_doc, cfg, rtf_paths, sels, output_path,
                             progress_cb = cb, hide_data)

  path <- paste0("houdini_log[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "].log")
  if(!quiet){
    lines <- write_log(input_doc, input_sheet, cfg ,sels, status, file_location)
    writeLines(lines, path)
  }


  invisible(status)
}

#ouputs the collected log data over a run
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
                   paste0("Timepoints: ", fmt_vec(sel$timelines)),
                   paste0("Excluded Columns: ", fmt_vec(sel$excluded_cols, none = "(none)")),
                   paste0("Excluded Rows: ", fmt_vec(sel$excluded_rows, none = "(none)"))

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

  lines
}




#' Watch an RTF folder and regenerate the document on every change
#'
#' Polls the RTF folder (and the config workbook, when given as a path) and
#' calls [houdini_run()] whenever a file appears, disappears, or changes.
#' Blocks until interrupted (Escape / Ctrl+C).
#'
#' @inheritParams apparate
#' @param interval Seconds between polls
#' @export
houdini_watch <- function(input_doc, input_sheet, file_location,
                          interval = 5) {
  snapshot <- function() {
    files <- list.files(file_location, pattern = "\\.rtf$", ignore.case = TRUE,
                        full.names = TRUE)
    if (is.character(input_sheet) && file.exists(input_sheet)) {
      files <- c(files, input_sheet)
    }
    paste(files, file.mtime(files), collapse = ";")
  }

  message("Watching ", file_location, " - press Escape or Ctrl+C to stop")
  last <- ""
  repeat {
    current <- snapshot()
    if (!identical(current, last)) {
      last <- current
      message("Change detected at ", format(Sys.time(), "%H:%M:%S"),
              " - regenerating")
      tryCatch(
        apparate(input_doc, input_sheet, file_location),
        error = function(e) message("Generation failed: ", conditionMessage(e))
      )
    }
    Sys.sleep(interval)
  }
}
