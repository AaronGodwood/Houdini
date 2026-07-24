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

  ui <- fluidPage(
    titlePanel("Houdini V2"),
    ui_input_panel()
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

    register_word_file()
    register_rtf_folder()

  }

  shiny::shinyApp(ui, server)

}
