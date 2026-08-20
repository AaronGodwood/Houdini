

# One finding against one config row.
finding <- function(row, severity, message, hint = NULL, bookmark = "", table = "") {
  list(row = row, severity = severity, message = message, hint = hint,
       bookmark = bookmark, table = table)
}

#' Validate a config against a document and its tables
#'
#' @param config data.frame with Bookmark and Table columns
#' @param bookmarks Named vector of bookmarks in the document, as returned by
#'   extract_bookmarks(); its "context" attribute is used when present
#' @param tables Character vector of table names available in the RTF folder
#' @param selections Per-row selections keyed by row index as character
#' @param info Named list of table_info_from_pages() results, keyed by table
#'   name. Rows whose table is absent here skip the filter checks.
#' @return A list of findings, each list(row, severity, message, hint,
#'   bookmark, table). Severity is "error" or "warning".
#' @keywords internal
validate_config <- function(config, bookmarks = character(), tables = character(),
                            selections = list(), info = list()) {
  n <- nrow(config)
  if (is.null(n) || n == 0L) return(list())

  bm_vals  <- trimws(as.character(config$Bookmark))
  tbl_vals <- trimws(as.character(config$Table))
  bm_vals[is.na(bm_vals)]   <- ""
  tbl_vals[is.na(tbl_vals)] <- ""

  ctx <- bookmarks_contexts(bookmarks)

  # Cross-row checks, computed once
  dup_bm  <- unique(bm_vals[nzchar(bm_vals)][duplicated(bm_vals[nzchar(bm_vals)])])
  dup_tbl <- unique(tbl_vals[nzchar(tbl_vals)][duplicated(tbl_vals[nzchar(tbl_vals)])])

  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- finding(...)

  for (i in seq_len(n)) {
    bm  <- bm_vals[i]
    tbl <- tbl_vals[i]

    # A wholly blank row is the grid's placeholder, not a mistake
    if (!nzchar(bm) && !nzchar(tbl)) next

    if (nzchar(bm) && !nzchar(tbl)) {
      add(i, "warning", "Bookmark set but no table selected.",
          "Name a table in the Dataset column for this row.", bm, tbl)
    }
    if (!nzchar(bm) && nzchar(tbl)) {
      add(i, "warning", "Table set but no bookmark selected.",
          "Name a bookmark in the Bookmark column for this row.", bm, tbl)
    }

    # Only meaningful once a document has actually been read
    if (nzchar(bm) && length(bookmarks) > 0L && !bm %in% names(bookmarks)) {
      e <- err_bookmark_missing(bm)
      add(i, "error", conditionMessage(e), e$hint, bm, tbl)
    }

    # [[ ]] errors rather than returning NULL when the name is absent, and a
    # document read without contexts has an empty vector here.
    this_ctx <- if (bm %in% names(ctx)) ctx[[bm]] else NULL
    if (nzchar(bm) && bm %in% names(bookmarks) &&
        !is.null(this_ctx) && !identical(this_ctx, "body")) {
      e <- err_bookmark_bad_context(bm, this_ctx)
      add(i, "error", conditionMessage(e), e$hint, bm, tbl)
    }

    # Two rows on one bookmark: the second injection replaces the first, so a
    # table goes missing from a run that otherwise looks successful.
    if (nzchar(bm) && bm %in% dup_bm) {
      e <- err_bookmark_duplicate(bm, which(bm_vals == bm))
      add(i, "error", conditionMessage(e), e$hint, bm, tbl)
    }

    if (nzchar(tbl) && length(tables) > 0L && !tbl %in% tables) {
      e <- err_rtf_unreadable(tbl, "not found in the table folder")
      add(i, "error", conditionMessage(e), e$hint, bm, tbl)
    }

    # Filter checks need the table to have been parsed
    ti  <- info[[tbl]]
    sel <- selections[[as.character(i)]]
    if (is.null(ti) || isTRUE(ti$is_image) || is.null(sel)) next

    # Validate only against a non-empty known set: a table with no parameters
    # detected cannot tell us whether a requested one is wrong.
    if (length(sel$parameters) > 0L && length(ti$parameters) > 0L) {
      bad <- setdiff(sel$parameters, ti$parameters)
      if (length(bad) > 0L) {
        add(i, "warning",
            paste0("Unknown parameter(s): ", paste(bad, collapse = ", "), "."),
            "These values were not found in the table. They may have been renamed or removed.",
            bm, tbl)
      }
    }
    if (length(sel$timelines) > 0L && length(ti$timelines) > 0L) {
      bad <- setdiff(sel$timelines, ti$timelines)
      if (length(bad) > 0L) {
        add(i, "warning",
            paste0("Unknown timepoint(s): ", paste(bad, collapse = ", "), "."),
            "These labels were not found in the table. Re-open it and reselect timepoints.",
            bm, tbl)
      }
    }
    if (length(sel$levels) > 0L && length(ti$levels) > 0L) {
      bad <- setdiff(sel$levels, ti$levels)
      if (length(bad) > 0L) {
        add(i, "warning",
            paste0("Unknown level(s): ", paste(bad, collapse = ", "), "."),
            "These level titles were not found in the table.", bm, tbl)
      }
    }
    if (length(sel$excluded_cols) > 0L && isTRUE(ti$n_cols > 0L)) {
      bad <- sel$excluded_cols[sel$excluded_cols < 1L |
                                 sel$excluded_cols > ti$n_cols]
      if (length(bad) > 0L) {
        e <- err_exclusion_out_of_range(bm, bad, ti$n_cols)
        add(i, "warning", conditionMessage(e), e$hint, bm, tbl)
      }
    }
  }

  out
}

# Gather the facts validate_config() needs by reading from disk.
# Parsing is best-effort: a table that will not parse is already reported as an
# error by the missing/unreadable checks, and should not stop the whole pass.
collect_validation_facts <- function(input_doc, config, rtf_paths) {
  bookmarks <- tryCatch(extract_bookmarks(input_doc),
                        error = function(e) character())

  wanted <- unique(trimws(as.character(config$Table)))
  wanted <- wanted[nzchar(wanted) & wanted %in% names(rtf_paths)]

  info <- list()
  for (nm in wanted) {
    info[[nm]] <- tryCatch({
      path <- rtf_paths[[nm]]
      if (isTRUE(is_image_rtf(path))) list(is_image = TRUE)
      else table_info_from_pages(parse_rtf(path))
    }, error = function(e) NULL)
  }

  list(bookmarks = bookmarks, tables = names(rtf_paths), info = info)
}

#' Check a configuration before generating a document
#'
#' Runs every check the app shows in its validation panel, without opening the
#' app and without writing anything. Use it in a script or CI step to fail
#' early, with the whole list of problems at once, rather than discovering them
#' one at a time during a run.
#'
#' The same rules drive the app's warnings panel, so a config that validates
#' cleanly here will validate cleanly there.
#'
#' @inheritParams apparate
#' @param quiet Suppress the printed summary; the findings are always returned
#' @return Invisibly, a data.frame of findings with one row per problem and
#'   columns `row`, `severity`, `bookmark`, `table`, `message` and `hint`. A
#'   clean config returns a zero-row data.frame.
#' @export
#' @examples
#' \dontrun{
#' # Fail a CI step on any error
#' problems <- houdini_validate("report.docx", "config.xlsx", "tables/")
#' if (any(problems$severity == "error")) stop("config is not ready")
#' }
houdini_validate <- function(input_doc, input_sheet, file_location,
                             figure_location = NULL, quiet = FALSE) {
  if (!file.exists(input_doc)) {
    stop(houdini_error(
      "docx_unreadable",
      paste0("Word document not found: ", input_doc),
      "Check the path to the .docx file",
      list(path = input_doc)
    ))
  }
  if (!dir.exists(file_location)) {
    stop(houdini_error(
      "rtf_folder_missing",
      paste0("RTF folder not found: ", file_location),
      "Check the path to the folder holding the rtf files",
      list(path = file_location)
    ))
  }
  if (is.null(figure_location) || !nzchar(trimws(figure_location))) {
    figure_location <- file_location
  } else if (!dir.exists(figure_location)) {
    stop(houdini_error(
      "rtf_folder_missing",
      paste0("Figure folder not found: ", figure_location),
      "Check the path to the folder holding the figure .rtf files, or leave it unset to use the table folder.",
      list(path = figure_location)
    ))
  }

  parsed <- if (is.character(input_sheet)) {
    read_xlsx(input_sheet)
  } else {
    tryCatch(
      parse_xl(input_sheet),
      houdini_error = function(e) stop(e),
      error = function(e) stop(err_excel_unreadable("(config data.frame", e))
    )
  }

  rtf_paths <- collect_rtf_paths(file_location, figure_location, warn = FALSE)
  facts <- collect_validation_facts(input_doc, parsed$config, rtf_paths)

  found <- validate_config(
    config     = parsed$config,
    bookmarks  = facts$bookmarks,
    tables     = facts$tables,
    selections = parsed$selections,
    info       = facts$info
  )

  out <- if (length(found) == 0L) {
    data.frame(row = integer(), severity = character(), bookmark = character(),
               table = character(), message = character(), hint = character(),
               stringsAsFactors = FALSE)
  } else {
    data.frame(
      row      = vapply(found, `[[`, integer(1),   "row"),
      severity = vapply(found, `[[`, character(1), "severity"),
      bookmark = vapply(found, `[[`, character(1), "bookmark"),
      table    = vapply(found, `[[`, character(1), "table"),
      message  = vapply(found, `[[`, character(1), "message"),
      hint     = vapply(found, function(f) f$hint %||% NA_character_,
                        character(1)),
      stringsAsFactors = FALSE
    )
  }

  if (!quiet) print_validation(out, nrow(parsed$config))
  invisible(out)
}

# Human-readable summary for the console.
print_validation <- function(found, n_rows) {
  n_err  <- sum(found$severity == "error")
  n_warn <- sum(found$severity == "warning")

  if (nrow(found) == 0L) {
    message(sprintf("Config is ready: %d row%s, no problems found.",
                    n_rows, if (n_rows == 1L) "" else "s"))
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(found))) {
    f <- found[i, ]
    where <- paste0("Row ", f$row,
                    if (nzchar(f$bookmark)) paste0(" - ", f$bookmark) else "",
                    if (nzchar(f$table))    paste0(" / ", f$table)    else "")
    message(sprintf("%s  %s\n  %s", toupper(f$severity), where, f$message))
    if (!is.na(f$hint)) message(sprintf("  Hint: %s", f$hint))
  }

  message(sprintf("\n%d error%s, %d warning%s across %d config row%s.",
                  n_err,  if (n_err  == 1L) "" else "s",
                  n_warn, if (n_warn == 1L) "" else "s",
                  n_rows, if (n_rows == 1L) "" else "s"))
  invisible(NULL)
}
