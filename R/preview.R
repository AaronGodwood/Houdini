# preview.R - look at a single RTF without launching the app.

# Escape text destined for page chrome rather than a table cell.
# htmlEscape() substitutes &nbsp for an empty string so cells keep their
# height, which is wrong for a title: it would render as a stray space.
escape_text <- function(x) {
  if (!nzchar(x)) return("")
  htmlEscape(x)
}

# Wrap a table or image fragment in a standalone HTML page.
preview_page <- function(title, body, subtitle = NULL) {
  sprintf(
    paste0(
      '<!DOCTYPE html><html><head><meta charset="utf-8">',
      '<title>%s</title><style>',
      'body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;',
      'margin:2rem auto;max-width:1100px;padding:0 1rem;color:#222}',
      'h1{font-size:1.1rem;margin:0 0 .2rem}',
      '.sub{color:#666;font-size:.85rem;margin-bottom:1.2rem}',
      '.frame{overflow-x:auto;border:1px solid #ddd;border-radius:4px;',
      'padding:1rem;background:#fff}',
      '</style></head><body><h1>%s</h1>%s<div class="frame">%s</div>',
      '</body></html>'
    ),
    escape_text(title), escape_text(title),
    if (is.null(subtitle) || !nzchar(subtitle)) "" else
      sprintf('<div class="sub">%s</div>', escape_text(subtitle)),
    body
  )
}

# An embedded PNG as an <img>, sized from the RTF's declared dimensions.
# Base64 keeps the page self-contained, matching the app's preview.
preview_image_html <- function(path, parameters = NULL) {
  imgs <- extract_pngs(path, parameters = parameters)$images
  if (length(imgs) == 0L) {
    stop(err_image_extract_failed(path, "no PNG data found"))
  }
  paste(vapply(imgs, function(img) {
    w_px <- if (!is.na(img$width_twips))  round(img$width_twips  * 96 / 1440) else NULL
    h_px <- if (!is.na(img$height_twips)) round(img$height_twips * 96 / 1440) else NULL
    sprintf(
      '<div style="text-align:center"><img src="data:image/png;base64,%s" style="max-width:100%%;height:auto;%s%s"></div>',
      base64enc::base64encode(img$png_bytes),
      if (is.null(w_px)) "" else sprintf("width:%dpx;", w_px),
      if (is.null(h_px)) "" else sprintf("height:%dpx;", h_px)
    )
  }, character(1)), collapse = "")
}

# One-line summary of what was parsed, shown under the title
preview_subtitle <- function(path, pages) {
  info <- table_info_from_pages(pages)
  # Multiplication sign and middle dot, written as escapes because R code in
  # a portable package has to be ASCII.
  bits <- sprintf("%d columns \u00d7 %d rows", info$n_cols, info$n_rows)
  if (length(pages) > 1L) {
    bits <- c(bits, sprintf("%d pages", length(pages)))
  }
  if (length(info$parameters) > 0L) {
    bits <- c(bits, sprintf("parameters: %s",
                            paste(info$parameters, collapse = ", ")))
  }
  if (length(info$timelines) > 0L) {
    bits <- c(bits, sprintf("timepoints: %s",
                            paste(info$timelines, collapse = ", ")))
  }
  if (length(info$levels) > 0L) {
    bits <- c(bits, sprintf("levels: %s", paste(info$levels, collapse = ", ")))
  }
  paste(bits, collapse = " \u00b7 ")
}

#' Preview a single RTF table or figure
#'
#' Renders one RTF file to a standalone HTML page and opens it, without
#' starting the Shiny app. Useful for checking how a table parses, confirming
#' that a filter selects what you expect, or inspecting a file that failed
#' during a run.
#'
#' Filters accept the same values as [apparate()]'s config columns, so a
#' selection can be tried here before being committed to a workbook.
#'
#' @param path Path to the .rtf file
#' @param parameters Character vector of parameter values to keep (NULL = all)
#' @param timelines Character vector of timepoint labels to keep (NULL = all)
#' @param levels Character vector of indent level titles to keep (NULL = all)
#' @param excluded_cols Integer vector of 1-based column indices to drop
#' @param excluded_rows Integer vector of data-row IDs to drop
#' @param hide_data Replace values with XX, for sharing a layout without data
#' @param browse Open the page in a browser or the RStudio viewer. When FALSE
#'   the file is written but not opened.
#' @param file Where to write the HTML. Defaults to a temporary file.
#' @return Invisibly, the path to the HTML file
#' @export
#' @examples
#' \dontrun{
#' houdini_preview("tables/t_14_1_1.rtf")
#' houdini_preview("tables/t_14_1_1.rtf", parameters = "Cholesterol")
#' }
houdini_preview <- function(path,
                            parameters    = NULL,
                            timelines     = NULL,
                            levels        = NULL,
                            excluded_cols = NULL,
                            excluded_rows = NULL,
                            hide_data     = FALSE,
                            browse        = interactive(),
                            file          = tempfile(fileext = ".html")) {
  if (!file.exists(path)) {
    stop(err_rtf_unreadable(path, "file does not exist"))
  }

  title <- basename(path)

  if (is_image_rtf(path)) {
    body <- preview_image_html(path, parameters = parameters)
    n_fig <- length(extract_pngs(path, parameters = parameters)$images)
    subtitle <- if (n_fig > 1L) sprintf("figure - %d pages", n_fig) else "figure"
  } else {
    pages <- parse_rtf(path, hide_data = hide_data)
    body  <- get_table_html_output(
      path,
      excluded_cols = excluded_cols,
      excluded_rows = excluded_rows,
      parameters    = parameters,
      timelines     = timelines,
      levels        = levels,
      pages         = pages,
      hide_data     = hide_data
    )
    subtitle <- preview_subtitle(path, pages)
  }

  writeLines(preview_page(title, body, subtitle), file, useBytes = TRUE)

  if (isTRUE(browse)) {
    # RStudio's pane is nicer when available, but a plain browser works
    # anywhere - and neither should turn a preview into an error.
    viewer <- getOption("viewer")
    if (is.function(viewer)) {
      tryCatch(viewer(file), error = function(e) browseURL(file))
    } else {
      browseURL(file)
    }
  }

  invisible(file)
}
