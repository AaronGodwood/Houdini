
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

  # Theme: Bootstrap 5
  houdini_theme <- bslib::bs_theme(
    version      = 5,
    preset       = "shiny",
    primary      = "#2c6e9b",
    success      = "#2a7a2a",
    base_font    = bslib::font_google("Inter", local = FALSE),
    heading_font = bslib::font_google("Inter", local = FALSE)
  )

  # App-specific CSS. Colours come from Bootstrap variables so both themes work.
  ui_head <- function() {
    tags$head(
      tags$style(HTML("
      /* Interactive preview: clickable headers/rows */
      .sel-pane th[data-col] { cursor: pointer; }
      .sel-pane tr[data-row] { cursor: pointer; }
      .preview-centred table { margin-left: auto; margin-right: auto; }
      /* Two stacked panes share the card body, each scrolling on its own.
         flex-basis 0 + equal grow keeps them balanced at any card height,
         and min-height 0 lets them actually shrink inside the flex parent. */
      .preview-scroll {
        overflow: auto;
        flex: 1 1 0;
        min-height: 120px;
        background: var(--bs-body-bg);
      }
      .pane-label {
        display: flex;
        align-items: baseline;
        gap: 0.25rem;
        padding: 0.3rem 0.6rem;
        font-size: 0.8em;
        font-weight: 600;
        color: var(--bs-secondary-color);
        background: var(--bs-tertiary-bg);
        position: sticky;
        top: 0;
        z-index: 2;
      }
      /* Preview tables inherit theme colours so dark mode stays readable */
      .preview-scroll table { color: var(--bs-body-color); }

      /* Compact toolbar above the config grid */
      .grid-toolbar {
        display: flex;
        flex-wrap: wrap;
        gap: 0.35rem;
        align-items: center;
        margin-bottom: 0.5rem;
      }
      .grid-toolbar .btn { --bs-btn-padding-y: 0.25rem; --bs-btn-padding-x: 0.5rem; }
      .grid-toolbar .sep {
        width: 1px; height: 1.4rem;
        background: var(--bs-border-color);
        margin: 0 0.25rem;
      }
      /* rhandsontable needs an explicit height to scroll internally */
      .grid-host { flex: 1 1 auto; min-height: 220px; overflow: auto; }

      /* Import Excel: hide the file-input slab, keep a normal-looking button */
      .compact-file .form-control,
      .compact-file .progress { display: none !important; }
      .compact-file .input-group { margin-bottom: 0 !important; }
      .compact-file .btn { --bs-btn-padding-y: 0.25rem; --bs-btn-padding-x: 0.5rem; }
      .compact-file { margin-bottom: 0 !important; }
      .compact-file .form-group { margin-bottom: 0 !important; }

      .doc-map {
        max-height: 260px; overflow-y: auto;
        border: 1px solid var(--bs-border-color);
        border-radius: var(--bs-border-radius);
        padding: 6px; font-size: 0.8em; line-height: 1.5;
        background: var(--bs-body-bg);
      }
      .doc-map .map-bm { cursor: pointer; border-radius: 3px; padding: 0 4px; }
      .doc-map .map-bm:hover { background: var(--bs-tertiary-bg); }

      .hint { font-size: 0.78em; color: var(--bs-secondary-color); }

      @keyframes spin { to { transform: rotate(360deg); } }
      .btn-spinner {
        display: inline-block; width: 14px; height: 14px;
        border: 2px solid rgba(255,255,255,0.4);
        border-top-color: #fff; border-radius: 50%;
        animation: spin 0.6s linear infinite;
        vertical-align: middle; margin-right: 6px;
      }
    ")),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('resetSelectionPane', function(_) {
        // Clear JS-side exclusion state so the re-rendered pane starts fresh
        Shiny.setInputValue('preview_excluded_cols',         [], {priority:'event'});
        Shiny.setInputValue('preview_excluded_rows',         [], {priority:'event'});
        Shiny.setInputValue('preview_excluded_header_rows',  [], {priority:'event'});
      });

      $(function() {
        var btn = document.getElementById('download_result');
        if (!btn) return;
        var origHTML = btn.innerHTML;
        btn.addEventListener('click', function() {
          btn.innerHTML = '<span class=\"btn-spinner\"></span>Generating...';
          btn.style.pointerEvents = 'none';
          btn.style.opacity = '0.75';
        });
        $(document).on('shiny:filedownload', function() {
          btn.innerHTML = origHTML;
          btn.style.pointerEvents = '';
          btn.style.opacity = '';
        });
      });
    "))
    )
  }

  # Sidebar: sources, generation, and the document map
  ui_sidebar <- function() {
    bslib::sidebar(
      width = 330,
      title = NULL,

      bslib::accordion(
        multiple = TRUE,
        open     = c("Sources", "Generate","Validation"),

        bslib::accordion_panel(
          "Sources", icon = bsicons::bs_icon("folder2-open"),
          tags$label(class = "form-label fw-semibold small", "Word document"),
          fileInput("word_file", NULL, accept = c(".docx", ".doc"),
                    placeholder = "Select .docx\u2026", width = "100%"),
          uiOutput("bookmark_status"),

          tags$label(class = "form-label fw-semibold small mt-2", "RTF folder"),
          uiOutput("folder_picker_btn"),
          uiOutput("pin_selector"),
          uiOutput("rtf_folder_input"),
          # textInput("rtf_folder_manual", NULL, width = "100%",
          #           placeholder = "Or paste a folder path\u2026"),
          uiOutput("rtf_status"),

        ),

        bslib::accordion_panel(
          "Generate", icon = bsicons::bs_icon("file-earmark-arrow-down"),
          downloadButton("download_result", "Download Result",
                         class = "btn-success w-100"),
          uiOutput("prebuild_status"),
          downloadButton("download_log", "Download Log",
                         class = "btn-outline-secondary btn-sm w-100 mt-2")
        ),

        # bslib::accordion_panel(
        #   "Document map", icon = bsicons::bs_icon("list-nested"),
        #   uiOutput("doc_map_panel")
        # ),

        bslib::accordion_panel(
          "Validation", icon = bsicons::bs_icon("exclamation-triangle"),
          uiOutput("row_warning_display")
        )
      )
    )
  }

  # Configuration card: search, toolbar, grid, filters
  ui_config_card <- function() {
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        span(bsicons::bs_icon("table"), " Table configuration"),
        uiOutput("config_summary", inline = TRUE)
      ),
      bslib::card_body(
        class = "d-flex flex-column",
        gap = "0.5rem",
        padding = "0.75rem",

        div(class = "d-flex gap-2 align-items-center",
            div(class = "flex-grow-1",
                textInput("grid_search", NULL, width = "100%",
                          placeholder = "Filter rows by bookmark or table\u2026")),
            actionButton("grid_search_clear", NULL,
                         icon = icon("times"), class = "btn-sm btn-outline-secondary",
                         title = "Clear filter")),

        div(class = "grid-toolbar",
            actionButton("add_row", NULL, icon = icon("plus"),
                         class = "btn-outline-secondary", title = "Add a row"),
            actionButton("remove_row", NULL, icon = icon("minus"),
                         class = "btn-outline-secondary",
                         title = "Remove the selected row"),
            div(class = "sep"),
            actionButton("fill_bookmarks", "All bookmarks",
                         class = "btn-outline-secondary",
                         title = "Add a row for each bookmark not already in the grid"),
            actionButton("auto_match", "Auto-match",
                         class = "btn-outline-primary",
                         title = "Fill blank cells with the best bookmark/table match"),
            div(class = "sep"),
            div(class = "compact-file d-inline-block",
                fileInput("import_excel", NULL, accept = ".xlsx",
                          buttonLabel = "Import", width = "auto")),
            downloadButton("export_excel", "Export",
                           class = "btn-outline-secondary btn-sm"),
            div(class = "ms-auto"),
            actionButton("clear_all", NULL, icon = icon("trash"),
                         class = "btn-outline-danger btn-sm",
                         title = "Clear the whole grid")
        ),

        div(class = "grid-host border rounded",
            rhandsontable::rHandsontableOutput("config_table")),

        uiOutput("filter_panel"),
        uiOutput("reset_row_btn")
      )
    )
  }

  # Preview card: Selection above Output, both live. Editing exclusions in the
  # Selection pane updates Output in place, so the two stay side-by-side in
  # time even though they are stacked in space.
  ui_preview_card <- function() {
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        uiOutput("preview_title", inline = TRUE),
        bslib::popover(
          actionLink("preview_opts", bsicons::bs_icon("gear")),
          title = "Preview options",
          checkboxInput("sel_show_all",
                        "Show all rows (large tables may be slow)",
                        value = FALSE),
          checkboxInput("sel_hide_data",
                        "Hide data values",
                        value = FALSE)
        )
      ),
      bslib::card_body(
        padding = 0,
        gap = 0,
        class = "d-flex flex-column",

        div(class = "pane-label",
            span(bsicons::bs_icon("hand-index"), " Selection"),
            span(class = "hint ms-2",
                 "click headers to exclude columns, rows to exclude rows")),
        div(class = "sel-pane preview-centred preview-scroll px-2 pb-2",
            uiOutput("selection_preview")),

        div(class = "pane-label border-top",
            span(bsicons::bs_icon("file-earmark-check"), " Output"),
            span(class = "hint ms-2", "exactly what will be injected")),
        div(class = "preview-centred preview-scroll px-2 pb-2",
            uiOutput("output_preview"))
      )
    )
  }

  ui <- bslib::page_sidebar(
    title = tagList(bsicons::bs_icon("magic"), "Houdini"),
    theme = houdini_theme,
    fillable = TRUE,
    ui_head(),
    sidebar = ui_sidebar(),
    bslib::layout_columns(
      col_widths = c(6, 6),

      ui_preview_card(),
      ui_config_card()
    )
  )

  server <- function(input, output, session) {

    # SHARED STATE - reactiveVals and helpers used across the register_* sections.
    # Each register_*() below is a local closure over this state; server() ends by
    # calling them all to wire up the app.

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

    #check if we are on posit connect or local as pins are only used on posit connect and not local devices
    is_connect <- function() {
      nzchar(Sys.getenv("CONNECT_CONTENT_GUID", "")) &&
        requireNamespace("pins", quietly = TRUE)
    }

    # Get cached parsed pages for a table, parsing on first access
    get_cached_pages <- function(tbl_name) {
      cache <- parse_cache()
      print("Step 1")
      if (!is.null(cache[[tbl_name]])) return(cache[[tbl_name]])
      paths <- rtf_paths()
      print("Step 12")
      if (!tbl_name %in% names(paths)) return(NULL)
      print("Step 165")
      pages <- parse_rtf(paths[[tbl_name]])
      cache[[tbl_name]] <- pages
      parse_cache(cache)
      pages
    }



    # selections keyed by row index (character):
    #   list(excluded_cols, excluded_rows, excluded_header_rows, parameters, timelines)
    table_selections    <- reactiveVal(list())
    last_gen_status     <- reactiveVal(NULL)


    # Per-row validation results; assigned in register_validation(), read by the
    # config grid renderer for the status column
    row_warnings <- NULL

    # Activate a config row: set the current table/row and warm the info cache.
    # Shared by the grid's select callback and the validation-panel jump links.
    activate_config_row <- function(row) {
      df <- config_data()
      if (is.null(row) || is.na(row) || row < 1 || row > nrow(df)) return()

      tbl_name <- df$Table[row]
      paths    <- rtf_paths()

      if (!is.null(tbl_name) && tbl_name != "" && tbl_name %in% names(paths)) {
        current_table_name(tbl_name)
        current_row_index(row)

        # Load table info if not cached (skip for image RTFs - no table to inspect)
        cache <- table_info_cache()
        if (is.null(cache[[tbl_name]])) {
          if (isTRUE(is_image_rtf(paths[[tbl_name]]))) {
            # Sentinel so the cache entry exists and current_info() is non-NULL
            cache[[tbl_name]] <- list(n_cols = 0L, n_rows = 0L,
                                      col_names  = character(),
                                      parameters = character(),
                                      timelines  = character(),
                                      levels     = character(),
                                      is_image = TRUE)
            table_info_cache(cache)
          } else {
            info <- tryCatch({
              table_info_from_pages(get_cached_pages(tbl_name))
            }, error = function(e) {
              showNotification(paste("Error reading RTF:", conditionMessage(e)),
                               type = "error")
              NULL
            })
            if (!is.null(info)) {
              cache[[tbl_name]] <- info
              table_info_cache(cache)
            }
          }
        }
      } else {
        current_table_name(NULL)
        current_row_index(NULL)
      }
    }



    #
    # WORD FILE

    register_word_file <- function() {
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
          div(class = "alert alert-success py-1 px-2 mb-0 small",
              bsicons::bs_icon("check-circle-fill"),
              sprintf(" %d bookmarks found", length(bm)))
        } else if (!is.null(input$word_file)) {
          div(class = "alert alert-warning py-1 px-2 mb-0 small",
              bsicons::bs_icon("exclamation-triangle-fill"), " No bookmarks found")
        }
      })
    }

    # RTF pin code for connect

    load_rtf_pin <- function(pin_name) {

      board <- pins::board_connect()

      rtf_files <- tryCatch(
        pins::pin_download(board, pin_name),
        error = function(e) {
          showNotification(
            paste("Failed to load pin:", e$message),
            type = "error"
          )
          return(NULL)
        }
      )

      rtf_folder_path(pin_name)


      if (length(rtf_files) == 0L || is.null(rtf_files)) {
        available_tables(character())
        showNotification(
          "No RTF files found in the selected pin.",
          type = "warning"
        )
        return()
      }

      tbl_names <- tools::file_path_sans_ext(basename(rtf_files))
      full_paths <- file.path(rtf_files)

      available_tables(tbl_names)
      rtf_paths(setNames(as.list(full_paths), tbl_names))

      table_info_cache(list())
      parse_cache(list())

      showNotification(
        paste("Found", length(rtf_files), "RTF files."),
        type = "message"
      )
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

    register_rtf_source <- function() {

      output$rtf_folder_input <- renderUI({
        if(is_connect()) return(NULL)
        textInput("rtf_folder_manual", NULL, width = "100%",
                  placeholder = "Or paste a folder path\u2026")
      })

      if(is_connect()){
        board <- pins::board_connect()

        output$pin_selector <- renderUI({

          pins_available <- tryCatch(
            pins::pin_list(board),
            error = function(e) character()
          )

          selectInput(
            "rtf_pin",
            "Choose RTF Pin",
            choices = pins_available,
            width = "100%"
          )
        })

        observeEvent(input$rtf_pin, {

          req(input$rtf_pin)

          load_rtf_pin(input$rtf_pin)

        }, ignoreInit = FALSE)

        output$rtf_status <- renderUI({

          tbls <- available_tables()

          if (length(tbls) > 0) {
            div(
              class = "alert alert-success py-1 px-2 mb-0 small",
              bsicons::bs_icon("check-circle-fill"),
              sprintf(" %d RTF files found", length(tbls))
            )
          }

        })

      }else{
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
            div(class = "alert alert-success py-1 px-2 mb-0 small",
                bsicons::bs_icon("check-circle-fill"),
                sprintf(" %d RTF files found", length(tbls)))
          }
        })
      }

    }

    # PREVIEW - row selection, filter panel, selection/output panes and the
    # observers that persist filters and exclusions per row.

    register_preview <- function() {

      observeEvent(input$config_table_select, {
        sel <- input$config_table_select
        if (is.null(sel)) return()

        # Grid rows are positions in the (possibly filtered) view - translate back
        # to the real config row index
        keep <- visible_rows()
        row  <- sel$select$r
        if (is.null(row) || is.na(row)) return()
        if (length(keep) < nrow(config_data()) && row >= 1L && row <= length(keep)) {
          row <- keep[row]
        }
        activate_config_row(row)
      })

      # Convenience reactive: current table info
      current_info <- reactive({
        tbl_name <- current_table_name()
        if (is.null(tbl_name)) return(NULL)
        table_info_cache()[[tbl_name]]
      })

      # FILTER PANEL

      output$filter_panel <- renderUI({
        info <- current_info()
        if (is.null(info) || isTRUE(info$is_image)) return(NULL)
        # NULL means "no filter" and must stay NULL: defaulting to the full set
        # here (and saving it back) would freeze the selection, silently dropping
        # any parameter/timeline added when the RTF is later regenerated.
        # An empty selectize renders its placeholder ("All parameters").
        prev        <- table_selections()[[as.character(current_row_index())]]
        prev_params   <- prev$parameters
        prev_tlines   <- prev$timelines
        prev_lvls     <- prev$levels
        prev_exc_cols <- prev$excluded_cols
        prev_exc_rows <- prev$excluded_rows
        prev_exc_hdrs <- prev$excluded_hdrs

        div(class = "border rounded p-2 mt-1",
            div(class = "fw-semibold small mb-1",
                bsicons::bs_icon("funnel"), " Filters"),

            if (length(info$parameters) > 0L) {
              selectizeInput(
                "sel_parameters", "Parameters:",
                choices  = info$parameters,
                selected = prev_params,
                multiple = TRUE,
                options  = list(plugins = list("remove_button"),
                                placeholder = "All parameters")
              )
            },

            if (length(info$timelines) > 0L) {
              selectizeInput(
                "sel_timelines", "Timepoints:",
                choices  = info$timelines,
                selected = prev_tlines,
                multiple = TRUE,
                options  = list(plugins = list("remove_button"),
                                placeholder = "All timepoints")
              )
            },

            if (length(info$levels) > 0L) {
              selectizeInput(
                "sel_levels", "Levels:",
                choices  = info$levels,
                selected = prev_lvls,
                multiple = TRUE,
                options  = list(plugins = list("remove_button"),
                                placeholder = "All levels")
              )
            },

            if (info$n_cols > 0L) {
              selectizeInput(
                "preview_excluded_cols", "Excluded Columns:",
                choices  = seq_len(info$n_cols),
                selected = prev_exc_cols,
                multiple = TRUE,
                options  = list(plugins = list("remove_button"),
                                placeholder = "No Columns Excluded")
              )
            },

            if (info$n_rows > 0L) {
              selectizeInput(
                "preview_excluded_rows", "Excluded Rows:",
                choices  = seq_len(info$n_rows),
                selected = prev_exc_rows,
                multiple = TRUE,
                options  = list(plugins = list("remove_button"),
                                placeholder = "No Rows Excluded")
              )
            },

            # if (info$n_hdrs > 0L) {
            #   selectizeInput(
            #     "preview_excluded_header_rows", "Excluded Headers:",
            #     choices  = seq_len(info$n_hdrs),
            #     selected = prev_exc_hdrs,
            #     multiple = TRUE,
            #     options  = list(plugins = list("remove_button"),
            #                     placeholder = "No Headers Excluded")
            #   )
            # }


        )
      })

      # Reset button - shown when a row with selections is active
      output$reset_row_btn <- renderUI({
        row <- current_row_index()
        if (is.null(row)) return(NULL)
        sel <- table_selections()[[as.character(row)]]
        has_selections <- !is.null(sel) && (
          length(sel$excluded_cols)        > 0L ||
            length(sel$excluded_rows)        > 0L ||
            length(sel$excluded_header_rows) > 0L ||
            !is.null(sel$parameters)         ||
            !is.null(sel$timelines)          ||
            !is.null(sel$levels)
        )
        if (!has_selections) return(NULL)
        div(style = "margin-top:6px;text-align:right;",
            actionLink("reset_row", "Reset selections for this row",
                       style = "font-size:0.8em;color:#888;"))
      })

      observeEvent(input$reset_row, {
        row <- current_row_index()
        if (is.null(row)) return()
        sels <- isolate(table_selections())
        sels[[as.character(row)]] <- list(
          excluded_cols        = integer(),
          excluded_rows        = integer(),
          excluded_header_rows = integer(),
          parameters           = NULL,
          timelines            = NULL,
          levels               = NULL
        )
        table_selections(sels)
        # Tell JS to clear its exclusion arrays and re-apply styles
        session$sendCustomMessage("resetSelectionPane", list())
      })

      # SAVE SELECTIONS WHEN INPUTS CHANGE

      # Save parameters/timelines when dropdowns change
      observeEvent(
        list(input$sel_parameters, input$sel_timelines, input$sel_levels),
        {

          row  <- isolate(current_row_index())
          info <- isolate(current_info())
          if (is.null(row) || is.null(info) || isTRUE(info$is_image)) return()

          sels <- isolate(table_selections())
          prev <- sels[[as.character(row)]] %||% list()
          sels[[as.character(row)]] <- list(
            excluded_cols        = prev$excluded_cols,
            excluded_rows        = prev$excluded_rows,
            excluded_header_rows = prev$excluded_header_rows,
            parameters           = input$sel_parameters,
            timelines            = input$sel_timelines,
            levels               = input$sel_levels
          )
          table_selections(sels)
        },
        ignoreNULL = FALSE
      )

      # Save exclusions when JS sends updated sets from the selection pane
      observeEvent(
        list(input$preview_excluded_cols,
             input$preview_excluded_rows,
             input$preview_excluded_header_rows),
        {
          row <- isolate(current_row_index())
          if (is.null(row)) return()

          sels <- isolate(table_selections())
          prev <- sels[[as.character(row)]] %||% list()
          sels[[as.character(row)]] <- list(
            excluded_cols        = as.integer(input$preview_excluded_cols        %||% integer()),
            excluded_rows        = as.integer(input$preview_excluded_rows        %||% integer()),
            excluded_header_rows = as.integer(input$preview_excluded_header_rows %||% integer()),
            parameters           = prev$parameters,
            timelines            = prev$timelines,
            levels               = prev$levels
          )
          table_selections(sels)
        },
        ignoreNULL = FALSE
      )

      # INTERACTIVE PREVIEW - selection pane + output pane

      # JS for the selection pane (injected after each render)
      selection_js <- '
(function() {
  var excCols = [], excRows = [], excHdrs = [];

  function toggle(arr, val) {
    var i = arr.indexOf(val);
    if (i === -1) arr.push(val); else arr.splice(i, 1);
  }

  function applyStyles(tbl) {
    tbl.querySelectorAll("tr[data-row]").forEach(function(tr) {
      var ri   = parseInt(tr.dataset.row, 10);
      var type = tr.dataset.rowtype;
      var excl = type === "header" ? excHdrs.indexOf(ri) !== -1
                                   : excRows.indexOf(ri) !== -1;
      tr.querySelectorAll("td,th").forEach(function(cell) {
        var raw = cell.dataset.col;
        var cols;
        try { cols = JSON.parse(raw); if (!Array.isArray(cols)) cols = [cols]; }
        catch(e) { cols = [parseInt(raw, 10)]; }
        var cExcl = cols.some(function(c) { return excCols.indexOf(c) !== -1; });
        cell.style.opacity = (excl || cExcl) ? "0.3" : "";
      });
      tr.style.opacity = excl ? "0.3" : "";
    });
  }

  var timer;
  function sendUpdate(tbl) {
    clearTimeout(timer);
    timer = setTimeout(function() {
      Shiny.setInputValue("preview_excluded_cols",
        excCols.slice().sort(function(a,b){return a-b;}), {priority:"event"});
      Shiny.setInputValue("preview_excluded_rows",
        excRows.slice().sort(function(a,b){return a-b;}), {priority:"event"});
      Shiny.setInputValue("preview_excluded_header_rows",
        excHdrs.slice().sort(function(a,b){return a-b;}), {priority:"event"});
    }, 300);
  }

  var container = document.getElementById("sel-pane-container");
  if (!container) return;
  try { excCols = JSON.parse(container.dataset.excCols || "[]"); } catch(e) {}
  try { excRows = JSON.parse(container.dataset.excRows || "[]"); } catch(e) {}
  try { excHdrs = JSON.parse(container.dataset.excHdrs || "[]"); } catch(e) {}

  var tbl = container.querySelector("table");
  if (!tbl) return;
  applyStyles(tbl);

  tbl.addEventListener("click", function(e) {
    var th = e.target.closest("th[data-col]");
    var tr = e.target.closest("tr[data-row]");
    if (th) {
      var raw = th.dataset.col;
      var cols;
      try { cols = JSON.parse(raw); if (!Array.isArray(cols)) cols = [cols]; }
      catch(err) { cols = [parseInt(raw, 10)]; }
      cols.forEach(function(c) { toggle(excCols, c); });
      applyStyles(tbl);
      sendUpdate(tbl);
      e.stopPropagation();
    } else if (tr) {
      var ri   = parseInt(tr.dataset.row, 10);
      var type = tr.dataset.rowtype;
      if (type === "header") toggle(excHdrs, ri); else toggle(excRows, ri);
      applyStyles(tbl);
      sendUpdate(tbl);
    }
  });
})();
'

      # Reactive that captures the "identity" of the current table+filters
      # (drives selection pane re-render - not per-click)
      selection_pane_key <- reactive({
        row <- current_row_index()
        sel <- if (!is.null(row)) table_selections()[[as.character(row)]] else NULL
        list(
          table  = current_table_name(),
          params = sel$parameters,
          tlines = sel$timelines,
          lvls   = sel$levels
        )
      })

      # Render an image RTF as a centred, scaled <img>
      image_preview_ui <- function(path) {
        img <- tryCatch(extract_png(path), error = function(e) NULL)
        if (is.null(img)) {
          return(div(class = "text-danger p-3", "Could not extract image."))
        }
        b64  <- paste0("data:image/png;base64,",
                       base64enc::base64encode(img$png_bytes))
        w_px <- if (!is.na(img$width_twips))  round(img$width_twips  * 96 / 1440) else NULL
        h_px <- if (!is.na(img$height_twips)) round(img$height_twips * 96 / 1440) else NULL
        tags$div(
          style = "text-align:center;",
          tags$img(src = b64, style = paste0(
            "max-width:100%;height:auto;",
            if (!is.null(w_px)) paste0("width:", w_px, "px;") else "",
            if (!is.null(h_px)) paste0("height:", h_px, "px;") else ""
          ))
        )
      }

      # Header line: which table, its shape, and any active filtering
      output$preview_title <- renderUI({
        tbl_name <- current_table_name()
        if (is.null(tbl_name)) {
          return(span(class = "text-secondary",
                      bsicons::bs_icon("eye"), " Preview"))
        }
        info <- current_info()
        row  <- current_row_index()
        sel  <- table_selections()[[as.character(row)]] %||% list()

        bits <- character()
        if (!is.null(info) && !isTRUE(info$is_image)) {
          bits <- c(bits, sprintf("%d cols \u00d7 %d rows", info$n_cols, info$n_rows))
        }
        n_excl <- length(sel$excluded_cols) + length(sel$excluded_rows) +
          length(sel$excluded_header_rows)
        if (n_excl > 0L) bits <- c(bits, sprintf("%d excluded", n_excl))
        if (!is.null(sel$parameters)) {
          bits <- c(bits, sprintf("%d param%s", length(sel$parameters),
                                  if (length(sel$parameters) == 1L) "" else "s"))
        }
        if (!is.null(sel$timelines)) {
          bits <- c(bits, sprintf("%d timeline%s", length(sel$timelines),
                                  if (length(sel$timelines) == 1L) "" else "s"))
        }
        if (!is.null(sel$levels)) {
          bits <- c(bits, sprintf("%d level%s", length(sel$levels),
                                  if (length(sel$levels) == 1L) "" else "s"))
        }

        tagList(
          span(class = "fw-semibold",
               if (isTRUE(info$is_image)) bsicons::bs_icon("image")
               else bsicons::bs_icon("table"),
               " ", tbl_name),
          if (length(bits) > 0L)
            span(class = "hint ms-2", paste(bits, collapse = " \u00b7 "))
        )
      })

      # Interactive pane: click headers/rows to exclude; excluded content is greyed
      # rather than removed. Re-renders only when the table or its filters change,
      # not on every click (the JS applies click feedback locally).
      output$selection_preview <- renderUI({

        key      <- selection_pane_key()
        tbl_name <- key$table
        paths    <- rtf_paths()
        info     <- current_info()


        if (is.null(tbl_name) || !tbl_name %in% names(paths)) {
          return(div(class = "text-secondary text-center p-4",
                     bsicons::bs_icon("hand-index"),
                     div(class = "mt-2", "Select a row to preview its table")))
        }

        if (isTRUE(info$is_image)) return(image_preview_ui(paths[[tbl_name]]))

        row <- current_row_index()
        sel <- table_selections()[[as.character(row)]] %||% list()
        ec  <- as.integer(sel$excluded_cols        %||% integer())
        er  <- as.integer(sel$excluded_rows        %||% integer())
        eh  <- as.integer(sel$excluded_header_rows %||% integer())

        int_to_json <- function(x) {
          if (length(x) == 0L) return("[]")
          paste0("[", paste(x, collapse = ","), "]")
        }

        html_content <- tryCatch(
          get_table_html_selection(
            paths[[tbl_name]],
            excluded_cols        = ec,
            excluded_rows        = er,
            excluded_header_rows = eh,
            parameters           = sel$parameters,
            timelines            = sel$timelines,
            levels               = sel$levels,
            pages                = get_cached_pages(tbl_name),
            row_limit            = if (isTRUE(input$sel_show_all)) Inf else 200L,
            hide_data            = if (isTRUE(input$sel_hide_data)) TRUE else FALSE
          ),
          error = function(e) sprintf(
            "<p class='text-danger'>Selection error: %s</p>",
            htmlEscape(conditionMessage(e))
          )
        )

        tagList(
          div(
            id = "sel-pane-container",
            `data-exc-cols` = int_to_json(ec),
            `data-exc-rows` = int_to_json(er),
            `data-exc-hdrs` = int_to_json(eh),
            HTML(html_content)
          ),
          tags$script(HTML(selection_js))
        )
      })

      # Result pane: exactly what will be injected. Driven by the debounced live
      # exclusions so it tracks clicks in the Selection pane without re-rendering
      # on every one; falls back to stored selections when the row changes.
      output$output_preview <- renderUI({
        tbl_name <- current_table_name()
        paths    <- rtf_paths()
        info     <- current_info()

        if (is.null(tbl_name) || !tbl_name %in% names(paths)) {
          return(div(class = "text-secondary text-center p-4", "No table selected"))
        }

        if (isTRUE(info$is_image)) return(image_preview_ui(paths[[tbl_name]]))

        row <- current_row_index()
        sel <- table_selections()[[as.character(row)]] %||% list()

        html_content <- tryCatch(
          get_table_html_output(
            paths[[tbl_name]],
            excluded_cols        = as.integer(sel$excluded_cols        %||% integer()),
            excluded_rows        = as.integer(sel$excluded_rows        %||% integer()),
            excluded_header_rows = as.integer(sel$excluded_header_rows %||% integer()),
            parameters           = sel$parameters,
            timelines            = sel$timelines,
            levels               = sel$levels,
            pages                = get_cached_pages(tbl_name),
            hide_data            = if (isTRUE(input$sel_hide_data)) TRUE else FALSE
          ),
          error = function(e) sprintf(
            "<p class='text-danger'>Output error: %s</p>",
            htmlEscape(conditionMessage(e))
          )
        )

        HTML(html_content)
      })
    }




    # CONFIG TABLE

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

    # Per-row status for the grid's icon column, encoded "icon|tooltip".
    # Generation outcome (from the last Download Result) wins for rows it
    # covers; otherwise live validation supplies an error/warning icon.
    row_status_values <- function(n) {
      warns <- row_warnings()
      gen   <- last_gen_status()
      vapply(seq_len(n), function(i) {
        key <- as.character(i)
        if (!is.null(gen) && key %in% names(gen)) {
          g <- gen[[key]]$err
          if (is.null(g)) return("\u2714|Injected successfully on last generation")
          msg <- if (inherits(g, "condition")) conditionMessage(g) else as.character(g)
          return(paste0("\u2716|", msg))
        }
        w <- if (i <= length(warns)) warns[[i]] else NULL
        if (!is.null(w) && nzchar(w$type)) {
          icon <- if (w$type == "error") "\u2716" else "\u26A0"
          return(paste0(icon, "|", w$msg))
        }
        "|"
      }, character(1))
    }

    status_renderer <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      var parts = String(value == null ? '' : value).split('|');
      td.innerHTML = '';
      td.textContent = parts[0];
      td.title = parts.slice(1).join('|');
      td.className = 'htCenter htMiddle';
      td.style.cursor = parts[1] ? 'help' : '';
      td.style.fontWeight = '700';
      if (parts[0] === '\\u2716')      td.style.color = '#c00000';
      else if (parts[0] === '\\u26a0') td.style.color = '#b8860b';
      else if (parts[0] === '\\u2714') td.style.color = '#2a7a2a';
      else                             td.style.color = '';
      return td;
    }"

    # rhandsontable 0.3.8's hot_col(renderer=) applies the renderer to EVERY
    # column, and all columns share one config object, so setting it on a single
    # index leaks too. Rebuild the list with per-column copies and attach the
    # renderer to just the target column.
    hot_renderer_one <- function(hot, col_index, renderer) {
      cols <- hot$x$columns
      cols <- lapply(seq_along(cols), function(i) {
        cc <- cols[[i]]
        cc[names(cc)] <- cc[names(cc)]     # force an independent copy
        if (i != col_index) cc$renderer <- NULL
        cc
      })
      cols[[col_index]]$renderer <- htmlwidgets::JS(renderer)
      hot$x$columns <- cols
      hot
    }

    # Rows currently visible in the grid, honouring the search box. Returns the
    # ORIGINAL row indices so selection, validation and selections stay keyed to
    # the real config rows rather than filtered positions.
    visible_rows <- reactive({
      df <- config_data()
      q  <- trimws(input$grid_search %||% "")
      if (nrow(df) == 0L || !nzchar(q)) return(seq_len(nrow(df)))
      hit <- grepl(q, df$Bookmark, ignore.case = TRUE, fixed = FALSE) |
        grepl(q, df$Table,    ignore.case = TRUE, fixed = FALSE)
      hit[is.na(hit)] <- FALSE
      which(hit)
    })

    register_config_grid <- function() {
      observeEvent(input$grid_search_clear, {
        updateTextInput(session, "grid_search", value = "")
      })

      output$config_summary <- renderUI({
        df <- config_data()
        n  <- nrow(df)
        complete <- sum(nzchar(trimws(df$Bookmark)) & nzchar(trimws(df$Table)))
        vis <- length(visible_rows())
        span(class = "hint",
             if (vis < n) sprintf("%d shown \u00b7 %d of %d mapped", vis, complete, n)
             else sprintf("%d of %d mapped", complete, n))
      })

      output$config_table <- rhandsontable::renderRHandsontable({
        df <- config_data()
        if (nrow(df) == 0) {
          df <- data.frame(Bookmark = rep("", 5), Table = rep("", 5),
                           stringsAsFactors = FALSE)
        }

        status <- row_status_values(nrow(df))
        keep   <- visible_rows()
        if (length(keep) == 0L) keep <- integer()

        df <- cbind(
          data.frame(` ` = status, check.names = FALSE, stringsAsFactors = FALSE),
          df
        )
        # Filtered view: show only matching rows, labelled with their real numbers
        if (length(keep) < nrow(df)) {
          df <- df[keep, , drop = FALSE]
          rownames(df) <- as.character(keep)
        }

        bm  <- available_bookmarks()
        tbl <- available_tables()

        hot <- rhandsontable::rhandsontable(df, rowHeaders = TRUE, selectCallback = TRUE,
                             overflow = "visible", height = 420) |>
          hot_cols(colWidths = c(30, 170, 170))

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

        # Status column last: read-only plus the isolated icon renderer
        hot <- hot |> hot_col(1, readOnly = TRUE)
        hot_renderer_one(hot, 1L, status_renderer)
      })

      observeEvent(input$config_table, {
        if (is.null(input$config_table)) return()
        edited <- hot_to_r(input$config_table)
        edited[[" "]] <- NULL          # status column is display-only
        edited$Status <- NULL          # tolerate either name

        # The grid may be showing a filtered subset; merge edits back into the
        # full config at their original row positions so hidden rows survive.
        full <- config_data()
        keep <- isolate(visible_rows())
        if (nrow(full) > 0L && length(keep) == nrow(edited) && length(keep) < nrow(full)) {
          full[keep, c("Bookmark", "Table")] <- edited[, c("Bookmark", "Table")]
          df <- full
        } else {
          df <- edited
        }

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
          # Remove the selected row if there is one, otherwise the last row
          removed <- current_row_index() %||% nrow(df)
          if (removed > nrow(df)) removed <- nrow(df)
          config_data(df[-removed, , drop = FALSE])
          # Selections are keyed by row index: drop the removed row's entry and
          # shift every later row's entry up one so they stay with their rows
          sels     <- table_selections()
          new_sels <- list()
          for (key in names(sels)) {
            k <- as.integer(key)
            if (k < removed) {
              new_sels[[key]] <- sels[[key]]
            } else if (k > removed) {
              new_sels[[as.character(k - 1L)]] <- sels[[key]]
            }
          }
          table_selections(new_sels)
          current_row_index(NULL)
          current_table_name(NULL)
        }
      })

      observeEvent(input$clear_all, {
        config_data(data.frame(Bookmark = rep("", 5), Table = rep("", 5),
                               stringsAsFactors = FALSE))
        table_selections(list())
        # Clear the active row too, or the preview panes keep showing a table
        # from a row that no longer exists
        current_row_index(NULL)
        current_table_name(NULL)
      })


      # EXCEL IMPORT

      observeEvent(input$import_excel, {
        req(input$import_excel)
        path <- input$import_excel$datapath

        xl <- tryCatch(
          read_xlsx(path),
          error = function(e) {
            showNotification(paste("Could not read Excel file:", conditionMessage(e)), type = "error")
            NULL
          }
        )
        if (is.null(xl)) return()

        # Fresh config and selections - discard any previous state
        config_data(xl$config)
        table_selections(xl$selections)
        showNotification(
          paste("Imported", nrow(xl$config), "rows from Excel"), type = "message"
        )
      })
    }

    # PER-ROW VALIDATION

    # Returns a list of length nrow(config_data()).
    # Each element: list(type = "error"|"warning"|"", msg = "plain text message")
    register_validation <- function() {
      row_warnings <<- reactive({
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

          # Bookmark exists but sits in a table cell or text box
          if (nzchar(bm_val) && bm_val %in% names(bm) &&
              !identical(bmctx[[bm_val]], "body") && !is.null(bmctx[[bm_val]])) {
            e <- err_bookmark_bad_context(bm_val, bmctx[[bm_val]])
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

          # Same RTF mapped to multiple bookmarks (warning only — may be intentional)
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
          cls <- if (is_error) "alert-danger" else "alert-warning"
          ico <- if (is_error) "x-octagon-fill" else "exclamation-triangle-fill"

          bm_val  <- trimws(df$Bookmark[i])
          tbl_val <- trimws(df$Table[i])
          row_lbl <- paste0("Row ", i,
                            if (nzchar(bm_val))  paste0(" \u2013 ", bm_val)  else "",
                            if (nzchar(tbl_val)) paste0(" / ", tbl_val) else "")

          # Clicking a warning jumps to the offending row
          div(class = paste("alert py-1 px-2 mb-1 small", cls),
              style = "cursor:pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('warning_row_click', %d, {priority:'event'})", i),
              div(class = "fw-semibold",
                  bsicons::bs_icon(ico), " ", row_lbl),
              div(w$msg),
              if (!is.null(w$hint))
                div(class = "fst-italic opacity-75", paste0("Hint: ", w$hint))
          )
        })

        items <- Filter(Negate(is.null), items)
        if (length(items) == 0L) {
          return(div(class = "hint",
                     bsicons::bs_icon("check-circle"), " No validation issues"))
        }

        div(style = "max-height:320px;overflow-y:auto;", items)
      })

      # Jump to the row a validation message refers to
      observeEvent(input$warning_row_click, {
        activate_config_row(input$warning_row_click)
      })
    }







    # DOWNLOADS - log, Excel export, and document generation handlers.

    register_downloads <- function() {
      output$download_log <- downloadHandler(
        filename = function() {
          # Colons are not accepted filenames on windows - (was failing to _)
          paste0("houdini_log[", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), "].log")
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

          sel_col <- function(field) {
            vapply(seq_len(nrow(df)), function(i) {
              semi_join(sels[[as.character(i)]][[field]])
            }, character(1))
          }

          out <- data.frame(
            Bookmark           = df$Bookmark,
            Dataset            = ifelse(nzchar(df$Table), paste0(df$Table, ".rtf"), df$Table),
            Parameters         = sel_col("parameters"),
            Timepoints         = sel_col("timelines"),
            Levels             = sel_col("levels"),
            ExcludedColumns    = sel_col("excluded_cols"),
            ExcludedRows       = sel_col("excluded_rows"),
            ExcludedHeaderRows = sel_col("excluded_header_rows"),
            stringsAsFactors   = FALSE
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

    register_word_file()
    register_rtf_source()
    register_config_grid()
    register_validation()
    register_preview()
    register_downloads()
  }

  shiny::shinyApp(ui, server)

}

