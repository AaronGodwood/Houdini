#' Return the Houdini Shiny application object
#'
#' Called internally by \code{\link{run_app}}. You can also pass the result
#' directly to \code{shiny::runApp()} if you need more control.
#'
#' @return A Shiny app object.
#' @import shiny
#' @import rhandsontable
#' @export
houdini_app <- function() {


  ui_input_panel <- function() {
    # Left panel: inputs
    column(3,
           wellPanel(
             h4("1. Word Document", class = "section-header"),
             fileInput("word_file", NULL,
                       accept = c(".docx", ".doc"),
                       placeholder = "Select Word file..."),
             uiOutput("bookmark_status"),

             h4("2. RTF Source", class = "section-header"),
             uiOutput("folder_picker_btn"),
             textInput("rtf_folder_manual", NULL,
                       placeholder = "Paste folder path and press Enter\u2026"),
             uiOutput("rtf_status"),

             h4("3. Generate", class = "section-header"),
             downloadButton("download_result", "Download Result",
                            class = "btn-success btn-lg btn-block"),
             downloadButton("download_log", "Download Log",
                            class = "btn-outline-secondary btn-block",
                            style = "margin-top:6px;"),

             uiOutput("row_warning_display")
           )
    )
  }

  ui_config_panel <- function() {
    # Middle panel: config table
    column(4,
           h4("Table Configuration", class = "section-header"),
           p("Map bookmarks to RTF tables. Click a row to configure filters and preview."),

           div(style = "height: 300px; overflow-y: auto; border: 1px solid #ddd; border-radius: 5px;",
               rHandsontableOutput("config_table")
           ),

           div(style = "display:flex;justify-content:space-between;align-items:flex-start;",
               div(class = "action-buttons",
                   actionButton("add_row",    "Add Row",    class = "btn-outline-secondary"),
                   actionButton("remove_row", "Remove Row", class = "btn-outline-secondary"),
                   actionButton("clear_all",  "Clear All",  class = "btn-outline-danger"),
                   actionButton("fill_bookmarks", "Add All Bookmarks",
                                class = "btn-outline-secondary",
                                title = "Add a row for each bookmark not already in the grid"),
                   actionButton("auto_match", "Auto-match",
                                class = "btn-outline-primary",
                                title = "Fill blank cells with the best bookmark/table match"),
                   fileInput("import_excel", NULL, accept = ".xlsx",
                             placeholder = "Import Excel...",
                             buttonLabel = "Import Excel",
                             width = "160px")
               ),
               downloadButton("export_excel", "Export Excel",
                              class = "btn-outline-secondary",
                              style = "margin-top:15px;")
           ),

           uiOutput("filter_panel"),
           uiOutput("reset_row_btn")
    )
  }

  ui <- fluidPage(
    titlePanel("Houdini V2"),
    ui_input_panel(),
    ui_config_panel()
  )

  server <- function(input, output, session) {

    # REACTIVE VALUES

    available_bookmarks <- reactiveVal(character())
    available_tables    <- reactiveVal(character())
    rtf_paths           <- reactiveVal(list())
    rtf_folder_path     <- reactiveVal(NULL)

    config_data <- reactiveVal(
      data.frame(
        Bookmark  = character(),
        Table = character(),
        stringsAsFactors = FALSE
      )
    )

    current_table_name <- reactiveVal(NULL)
    current_row_index  <- reactiveVal(NULL)

    # table_info cache: table_name -> get_table_info() result
    table_info_cache <- reactiveVal(list())

    # parse_rtf cache: table_name -> parse_rtf() result (list of pages)
    parse_cache <- reactiveVal(list())

    # Get cached parsed pages for a table, parsing on first access
    get_cached_pages <- function(tbl_name) {
      cache <- parse_cache()
      if (!is.null(cache[[tbl_name]])) return(cache[[tbl_name]])
      paths <- rtf_paths()
      if (!tbl_name %in% names(paths)) return(NULL)
      pages <- parse_rtf(paths[[tbl_name]])
      cache[[tbl_name]] <- pages
      parse_cache(cache)
      pages
    }

    # selections keyed by row index (character):
    #   list(excluded_cols, excluded_rows, excluded_header_rows, parameters, timelines)
    table_selections    <- reactiveVal(list())
    last_gen_status     <- reactiveVal(NULL)

    register_word_file <- function(){
      observeEvent(input$word_file, {
        req(input$word_file)
        bookmarks <- tryCatch(
          extract_bookmarks(input$word_file$datapath),
          error = function(e) {
            showNotification(paste("Bookmark error:", conditionMessage(e)), type = "error")
            character()
          }
        )

        available_bookmarks(bookmarks)
      })


      output$bookmark_status <- renderUI({
        bm <- available_bookmarks()
        if (length(bm) > 0) {
          div(class = "status-box status-success",
              icon("check-circle"), paste(length(bm), "bookmarks found"))
        } else if (!is.null(input$word_file)) {
          div(class = "status-box status-warning",
              icon("exclamation-triangle"), "No bookmarks found")
        }
      })
    }

    # RTF FOLDER

    # Shared helper: load RTF files from a validated folder path
    load_rtf_folder <- function(folder) {
      folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)
      if (!dir.exists(folder)) {
        showNotification("Folder not found. Check the path and try again.", type = "error")
        return()
      }
      rtf_folder_path(folder)
      rtf_files <- list.files(folder, pattern = "\\.rtf$", ignore.case = TRUE)
      if (length(rtf_files) == 0L) {
        available_tables(character())
        showNotification("No RTF files found in that folder.", type = "warning")
        return()
      }
      tbl_names  <- tools::file_path_sans_ext(rtf_files)
      full_paths <- file.path(folder, rtf_files)
      available_tables(tbl_names)
      rtf_paths(setNames(as.list(full_paths), tbl_names))
      table_info_cache(list())
      parse_cache(list())
      showNotification(paste("Found", length(rtf_files), "RTF files."), type = "message")
    }

    register_rtf_folder <- function(){
      # Show native folder picker button only when inside RStudio Desktop
      # (rstudioapi is in Suggests, so check it is installed before calling it)
      output$folder_picker_btn <- renderUI({
        if (!requireNamespace("rstudioapi", quietly = TRUE)) return(NULL)
        if (!isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) return(NULL)
        actionButton("pick_folder_rstudio", "Choose RTF Folder\u2026",
                     class = "btn-primary",
                     style = "width:100%;")
      })

      observeEvent(input$pick_folder_rstudio, {
        folder <- tryCatch(
          rstudioapi::selectDirectory(caption = "Select RTF folder"),
          error = function(e) NULL
        )
        if (is.null(folder) || !nzchar(folder)) return()
        load_rtf_folder(folder)
        updateTextInput(session, "rtf_folder_manual", value = folder)
      })

      # Handle manual path entry — fires on Enter (input value change)
      observeEvent(input$rtf_folder_manual, {
        path <- trimws(input$rtf_folder_manual)
        if (!nzchar(path)) return()
        load_rtf_folder(path)
      }, ignoreInit = TRUE)

      output$rtf_status <- renderUI({
        tbls <- available_tables()
        if (length(tbls) > 0) {
          div(class = "status-box status-success",
              icon("check-circle"), paste(length(tbls), "RTF files found"))
        }
      })
    }


    # Fill blank Bookmark/Table cells with their best fuzzy match against the
    # loaded bookmark names / RTF table names. Never overwrites a non-empty cell.
    # score_floor guards against filling unrelated sheets with noise (bulk button);
    # pass 0 to always take the best guess (live per-row suggestion).
    fill_suggestions <- function(df, score_floor = 0) {
      bm_names <- names(available_bookmarks())
      tbls     <- available_tables()
      if (length(bm_names) == 0L && length(tbls) == 0L) return(df)

      for (i in seq_len(nrow(df))) {
        b <- trimws(df$Bookmark[i])
        t <- trimws(df$Table[i])

        if (nzchar(b) && !nzchar(t) && length(tbls) > 0L) {
          m <- best_match(b, tbls)
          if (!is.na(m$match) && m$score >= score_floor) df$Table[i] <- m$match
        } else if (nzchar(t) && !nzchar(b) && length(bm_names) > 0L) {
          m <- best_match(t, bm_names)
          if (!is.na(m$match) && m$score >= score_floor) df$Bookmark[i] <- m$match
        }
      }
      df
    }



    register_config_grid <- function(){
      output$config_table <- renderRHandsontable({
        df <- config_data()
        if (nrow(df) == 0) {
          df <- data.frame(Bookmark = rep("", 5), Table = rep("", 5),
                           stringsAsFactors = FALSE)
        }

        bm  <- available_bookmarks()
        tbl <- available_tables()

        hot <- rhandsontable(df, rowHeaders = TRUE, selectCallback = TRUE,
                             overflow = "visible") |>
          hot_cols(colWidths = c(160, 160))

        hot <- if (length(bm) > 0) {
          hot |> hot_col("Bookmark", type = "dropdown", source = c("", names(bm)), strict = FALSE)
        } else {
          hot |> hot_col("Bookmark", type = "text")
        }

        hot <- if (length(tbl) > 0) {
          hot |> hot_col("Table", type = "dropdown", source = c("", tbl), strict = FALSE)
        } else {
          hot |> hot_col("Table", type = "text")
        }

        hot
      })

      observeEvent(input$config_table, {
        if (is.null(input$config_table)) return()
        df <- hot_to_r(input$config_table)
        # Live per-row suggestion: fill a blank partner cell for any row where the
        # other cell was just set. Blank-only + change-detection keeps this stable
        # (the re-render feeds back through this observer but produces no new change).
        suggested <- fill_suggestions(df, score_floor = 0)
        if (!identical(suggested, df)) df <- suggested
        config_data(df)
      })

      observeEvent(input$auto_match, {
        df <- config_data()
        if (length(available_bookmarks()) == 0L && length(available_tables()) == 0L) {
          showNotification("Load a Word document and RTF folder first", type = "warning")
          return()
        }
        filled <- fill_suggestions(df, score_floor = 0.12)
        n_new  <- sum(nzchar(trimws(unlist(filled[c("Bookmark", "Table")]))) &
                        !nzchar(trimws(unlist(df[c("Bookmark", "Table")]))))
        config_data(filled)
        showNotification(
          if (n_new > 0L) sprintf("Auto-match filled %d cell%s", n_new,
                                  if (n_new == 1L) "" else "s")
          else "No confident matches to fill",
          type = if (n_new > 0L) "message" else "default"
        )
      })

      observeEvent(input$fill_bookmarks, {

        bm_names <- names(available_bookmarks())[grepl("(^[Tt]able)|(^[Ff]igure)",names(available_bookmarks()))]
        if (length(bm_names) == 0L) {
          showNotification("Load a Word document with bookmarks first", type = "warning")
          return()
        }

        df       <- config_data()
        existing <- trimws(df$Bookmark)
        missing  <- setdiff(bm_names, existing[nzchar(existing)])
        if (length(missing) == 0L) {
          showNotification("All bookmarks are already in the grid", type = "default")
          return()
        }

        # Reuse rows that have a blank Bookmark first, then append the remainder,
        # so we don't leave the initial placeholder rows empty above the new ones.
        blank_rows <- which(!nzchar(existing))
        n_reuse    <- min(length(blank_rows), length(missing))
        if (n_reuse > 0L) {
          df$Bookmark[blank_rows[seq_len(n_reuse)]] <- missing[seq_len(n_reuse)]
        }
        remainder <- missing[seq_len(length(missing) - n_reuse) + n_reuse]
        if (length(remainder) > 0L) {
          df <- rbind(df, data.frame(Bookmark = remainder, Table = "",
                                     stringsAsFactors = FALSE))
        }

        config_data(df)
        showNotification(
          sprintf("Added %d bookmark%s", length(missing),
                  if (length(missing) == 1L) "" else "s"),
          type = "message"
        )
      })

      observeEvent(input$add_row, {
        df <- config_data()
        config_data(rbind(df, data.frame(Bookmark = "", Table = "",
                                         stringsAsFactors = FALSE)))
      })

      observeEvent(input$remove_row, {
        df <- config_data()
        if (nrow(df) > 1) {
          removed <- nrow(df)
          config_data(df[-removed, , drop = FALSE])
          # Drop the removed row's selections so a later Add Row doesn't inherit them
          sels <- table_selections()
          if (!is.null(sels[[as.character(removed)]])) {
            sels[[as.character(removed)]] <- NULL
            table_selections(sels)
          }
          if (identical(current_row_index(), removed)) {
            current_row_index(NULL)
            current_table_name(NULL)
          }
        }
      })

      observeEvent(input$clear_all, {
        config_data(data.frame(Bookmark = rep("", 5), Table = rep("", 5),
                               stringsAsFactors = FALSE))
        table_selections(list())
      })


      # EXCEL IMPORT

      observeEvent(input$import_excel, {
        req(input$import_excel)
        path <- input$import_excel$datapath

        imported <- read_xlsx(path)

        new_config <- imported$config
        config_data(new_config)


        table_selections(imported$selections)
        showNotification(
          paste("Imported", nrow(new_config), "rows from Excel"), type = "message"
        )
      })
    }





    register_downloads <- function(){
      # LOG DOWNLOAD

      output$download_log <- downloadHandler(
        filename = function() {
          paste0("houdini_log[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "].log")
        },
        content = function(file) {
          df    <- config_data()
          sels  <- table_selections()
          paths <- rtf_paths()
          status <- last_gen_status()

          lines <- write_log(input$word_file$name, "Imported File", df, sels, status, rtf_folder_path())
          writeLines(lines, file)
        }
      )

      # EXCEL EXPORT

      output$export_excel <- downloadHandler(
        filename = function() {
          paste0("houdini_config_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        },
        content = function(file) {
          df   <- config_data()
          sels <- table_selections()

          semi_join <- function(x) if (length(x) == 0L || is.null(x)) "" else paste(x, collapse = "; ")

          out <- data.frame(
            Bookmark   = df$Bookmark,
            Table      = ifelse(nzchar(df$Table), paste0(df$Table, ".rtf"), df$Table),
            Parameters = vapply(seq_len(nrow(df)), function(i) {
              semi_join(sels[[as.character(i)]]$parameters)
            }, character(1)),
            Timelines  = vapply(seq_len(nrow(df)), function(i) {
              semi_join(sels[[as.character(i)]]$timelines)
            }, character(1)),
            stringsAsFactors = FALSE
          )

          writexl::write_xlsx(out, file)
        }
      )

      # DOCUMENT GENERATION

      output$download_result <- downloadHandler(
        filename = function() {
          if (!is.null(input$word_file)) paste0("Houdini_Output_",input$word_file$name)
          else "output.docx"
        },
        content = function(file) {
          req(input$word_file)

          config <- config_data()
          keep   <- which(nzchar(trimws(config$Bookmark)) & nzchar(trimws(config$Table)))
          config <- config[keep, , drop = FALSE]

          if (nrow(config) == 0L) {
            showNotification("No table mappings defined", type = "error"); return()
          }

          # Selections are keyed by original grid row index; re-key them to match
          # the filtered config so blank rows above don't shift them onto the
          # wrong tables.
          all_sels <- table_selections()
          selections <- setNames(
            lapply(keep, function(i) all_sels[[as.character(i)]]),
            as.character(seq_along(keep))
          )

          n_rows <- nrow(config)
          status <- tryCatch(
            withProgress(
              message = "Generating document\u2026",
              value   = 0,
              {
                process_document(
                  word_path   = input$word_file$datapath,
                  config      = config,
                  rtf_paths   = rtf_paths(),
                  selections  = selections,
                  output_path = file,
                  progress_cb = function(i, n, msg) {
                    incProgress(1 / n, detail = msg)
                  }
                )
              }
            ),
            error = function(e) {
              showNotification(paste("Error generating document:", conditionMessage(e)), type = "error")
              NULL
            }
          )
          # process_document keys status by filtered row position; map back to
          # original grid rows so the log pairs errors with the right rows
          if (!is.null(status)) names(status) <- as.character(keep)
          last_gen_status(status)
        }
      )
    }


    register_validation <- function(){
      # PER-ROW VALIDATION

      # Returns a list of length nrow(config_data()).
      # Each element: list(type = "error"|"warning"|"", msg = "plain text message")
      row_warnings <- reactive({
        df    <- config_data()
        bm    <- available_bookmarks()
        bmctx <- bookmarks_contexts(bm)
        tbls  <- available_tables()
        paths <- rtf_paths()
        cache <- table_info_cache()
        sels  <- table_selections()

        # Pre-compute duplicate bookmark and table name sets (cross-row checks)
        all_bm_vals  <- trimws(df$Bookmark)
        all_tbl_vals <- trimws(df$Table)
        nonempty_bms  <- all_bm_vals[nzchar(all_bm_vals)]
        nonempty_tbls <- all_tbl_vals[nzchar(all_tbl_vals)]
        dup_bookmarks <- unique(nonempty_bms[duplicated(nonempty_bms)])
        dup_tables    <- unique(nonempty_tbls[duplicated(nonempty_tbls)])

        lapply(seq_len(nrow(df)), function(i) {
          bm_val  <- trimws(df$Bookmark[i])
          tbl_val <- trimws(df$Table[i])

          if (!nzchar(bm_val) && !nzchar(tbl_val)) return(list(type = "", msg = "", hint = NULL))

          errors   <- character()
          warnings <- character()
          hints    <- character()

          # Half-complete row
          if (nzchar(bm_val) && !nzchar(tbl_val)) {
            warnings <- c(warnings, "Bookmark set but no table selected")
            hints    <- c(hints, "Select an RTF table in the Table column for this row.")
          }
          if (!nzchar(bm_val) && nzchar(tbl_val)) {
            warnings <- c(warnings, "Table set but no bookmark selected")
            hints    <- c(hints, "Select a bookmark in the Bookmark column for this row.")
          }

          # Bookmark not in Word document
          if (nzchar(bm_val) && length(bm) > 0L && !bm_val %in% names(bm)) {
            e <- err_bookmark_missing(bm_val)
            errors <- c(errors, conditionMessage(e))
            hints  <- c(hints,  e$hint)
          }

          #Bookmark exists but sits in a table cell
          if(nzchar(bm_val) && bm_val %in% names(bm) && !identical(bmctx[[bm_val]], "body") && !is.null(bmctx[[bm_val]])){
            e <- err_bookmark_bad_context(bm_val,bmctx[[bm_val]])
            errors <- c(errors, conditionMessage(e))
            hints  <- c(hints,  e$hint)
          }

          # Duplicate bookmark across rows
          if (nzchar(bm_val) && bm_val %in% dup_bookmarks) {
            dup_rows <- which(all_bm_vals == bm_val)
            e <- err_bookmark_duplicate(bm_val, dup_rows)
            errors <- c(errors, conditionMessage(e))
            hints  <- c(hints,  e$hint)
          }

          # Table not in RTF folder
          if (nzchar(tbl_val) && length(tbls) > 0L && !tbl_val %in% tbls) {
            e <- err_rtf_unreadable(tbl_val, "not found in RTF folder")
            errors <- c(errors, conditionMessage(e))
            hints  <- c(hints,  e$hint)
          }

          # Same RTF mapped to multiple bookmarks (warning only - may be intentional)
          if (nzchar(tbl_val) && tbl_val %in% dup_tables) {
            dup_rows <- which(all_tbl_vals == tbl_val)
            warnings <- c(warnings, sprintf(
              "Table '%s' is mapped in multiple rows: %s",
              tbl_val, paste(dup_rows, collapse = ", ")
            ))
            hints <- c(hints, "This is allowed but unusual. Verify that both bookmarks should receive the same table.")
          }

          if (nzchar(tbl_val) && length(paths) > 0L && tbl_val %in% names(paths)) {
            info <- cache[[tbl_val]]
            sel  <- sels[[as.character(i)]]

            if (!is.null(info) && !isTRUE(info$is_image)) {
              # Only validate against a non-empty known list - if the table has no
              # parameters/timelines detected, we can't meaningfully validate imports
              if (!is.null(sel$parameters) && length(sel$parameters) > 0L &&
                  length(info$parameters) > 0L) {
                bad_p <- setdiff(sel$parameters, info$parameters)
                if (length(bad_p) > 0L) {
                  warnings <- c(warnings,
                                paste0("Unknown parameter(s): ", paste(bad_p, collapse = ", ")))
                  hints <- c(hints,
                             "These parameter values were not found in the RTF file. They may have been renamed or removed.")
                }
              }
              if (!is.null(sel$timelines) && length(sel$timelines) > 0L &&
                  length(info$timelines) > 0L) {
                bad_t <- setdiff(sel$timelines, info$timelines)
                if (length(bad_t) > 0L) {
                  warnings <- c(warnings,
                                paste0("Unknown timeline(s): ", paste(bad_t, collapse = ", ")))
                  hints <- c(hints,
                             "These timeline labels were not found in the RTF file. Re-open the table and reselect timelines.")
                }
              }
              if (!is.null(sel$excluded_cols) && length(sel$excluded_cols) > 0L &&
                  info$n_cols > 0L) {
                bad_c <- sel$excluded_cols[
                  sel$excluded_cols < 1L | sel$excluded_cols > info$n_cols
                ]
                if (length(bad_c) > 0L) {
                  e <- err_exclusion_out_of_range(bm_val, bad_c, info$n_cols)
                  warnings <- c(warnings, conditionMessage(e))
                  hints    <- c(hints,    e$hint)
                }
              }
            }
          }

          hint_str <- if (length(hints) > 0L) paste(unique(hints), collapse = " ") else NULL

          if (length(errors) > 0L)
            return(list(type = "error",   msg = paste(errors,   collapse = "; "), hint = hint_str))
          if (length(warnings) > 0L)
            return(list(type = "warning", msg = paste(warnings, collapse = "; "), hint = hint_str))

          list(type = "", msg = "", hint = NULL)
        })
      })

      # Scrollable warnings panel - all rows with issues, shown below the preview
      output$row_warning_display <- renderUI({
        warns <- row_warnings()
        df    <- config_data()

        items <- lapply(seq_along(warns), function(i) {
          w <- warns[[i]]
          if (!nzchar(w$type)) return(NULL)

          is_error <- w$type == "error"
          bg  <- if (is_error) "#f8d7da" else "#fff3cd"
          bdr <- if (is_error) "#f5c2c7" else "#ffecb5"
          ico <- if (is_error) "\u26a0" else "\u26a0"
          lbl <- if (is_error) "Error" else "Warning"

          bm_val  <- trimws(df$Bookmark[i])
          tbl_val <- trimws(df$Table[i])
          row_lbl <- paste0("Row ", i,
                            if (nzchar(bm_val))  paste0(" \u2013 ", bm_val)  else "",
                            if (nzchar(tbl_val)) paste0(" / ", tbl_val) else "")

          div(style = sprintf(
            "padding:5px 8px;margin-bottom:4px;border-radius:4px;
             background:%s;border:1px solid %s;font-size:0.82em;", bg, bdr),
            strong(paste0(ico, " ", lbl, ": ")), row_lbl,
            tags$br(),
            span(style = "color:#555;", w$msg),
            if (!is.null(w$hint)) tagList(tags$br(),
                                          span(style = "color:#777;font-style:italic;", paste0("Hint: ", w$hint))
            )
          )
        })

        items <- Filter(Negate(is.null), items)
        if (length(items) == 0L) return(NULL)

        div(style = "margin-top:12px;max-height:350px;overflow-y:auto;",
            h5(style = "margin:0 0 6px;font-size:0.9em;color:#666;", "Validation warnings"),
            items)
      })
    }

    register_word_file()
    register_rtf_folder()
    register_config_grid()
    register_validation()
    register_downloads()

  }

  shiny::shinyApp(ui, server)

}
