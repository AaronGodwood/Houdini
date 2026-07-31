# tests/make_test_rtfs.R
#
# Generates basic synthetic RTF files matching the expected SAS output format.
# Run this script to produce test fixtures in a specified directory.
#
# Usage:
#   source("tests/make_test_rtfs.R")
#   make_test_rtfs("C:/path/to/output/folder")


# LOW-LEVEL RTF BUILDING HELPERS


# Convenience constructor so all cells have consistent named fields
cell <- function(text, width_twips, align = "c", bold = FALSE,
                 merge_first = FALSE, merge_cont = FALSE) {
  list(text        = as.character(text),
       width_twips = as.integer(width_twips),# strip any names
       align       = align,
       bold        = bold,
       merge_first = merge_first,
       merge_cont  = merge_cont)
}

# One RTF table row.
# cells: list of cell() objects
# trhdr: TRUE if this is a header row (\trhdr)
rtf_row <- function(cells, trhdr = FALSE) {
  trhdr_tag  <- if (trhdr) "\\trhdr" else ""
  trhdr_brds <- if (trhdr) "\\clbrdrb\\brdrs\\brdrw19\\brdrcf1" else ""

  # Cell definitions: merge flags then \cellx boundary
  boundary  <- 0L
  cell_defs <- paste(vapply(cells, function(c) {
    boundary  <<- boundary + c$width_twips
    sprintf("%s\\cltxlrtb\\clvertalt\\clcbpat8\\clpadt29\\clpadft3\\clpadr29\\clpadfr3\\cellx%d\r\n", trhdr_brds, boundary)
  }, character(1)), collapse = "")


  # Cell contents
  cell_contents <- paste(vapply(cells, function(c) {
    bold_on  <- if (isTRUE(c$bold)) "\\b"  else ""
    align    <- c$align
    sprintf("\\pard\\plain\\intbl%s\\sb29\\sa29\\q%s\\f1\\fs20\\cf1{%s\\cell}\r\n", bold_on, align, c$text)
  }, character(1)), collapse = "")

  sprintf("\\trowd\\trkeep%s\\trqc\r\n%s%s{\\row}\r\n", trhdr_tag, cell_defs, cell_contents)
}

# RTF header section containing a parameter line in its bottom row
rtf_header_section <- function(parameter = NULL, page_width_twips = 12240L,
                               margin_twips = 1800L) {
  text_width <- page_width_twips - 2L * margin_twips  # 8640 twips

  # Top row: study/table title (purely decorative, tests robustness)
  title_row <- rtf_row(list(
    list(text = "Study XYZ - Summary Table", width_twips = text_width, bold = TRUE, align = "c")
  ), trhdr = FALSE)

  # Bottom row: parameter line (or blank if no parameter)
  param_text <- if (!is.null(parameter)) sprintf("Parameter: %s", parameter) else ""
  param_row  <- rtf_row(list(
    list(text = param_text, width_twips = text_width, bold = FALSE, align = "l")
  ), trhdr = FALSE)

  sprintf("{\\header %s%s}", title_row, param_row)
}

# RTF footer section
rtf_footer_section <- function(page_num_text = "Page 1") {
  sprintf("{\\footer {\\pard %s\\par}}", page_num_text)
}

# Section definition block (page setup for A4 portrait with standard margins)
# pgwsxn=12240 (8.5in), pghsxn=15840 (11in), margl/r=1800 (1.25in), margt/b=1440 (1in)
rtf_sectd <- function() {
  "\\sectd\\linex0\\endnhere\\pgwsxn15840\\pghsxn12240\\lndscpsxn\\headery1440\\footery873\\marglsxn1440\\margrsxn1440\\margtsxn1440\\margbsxn873\r\n"
}

# Wrap pages into a complete RTF document
rtf_document <- function(pages) {
  page_blocks <- paste(vapply(seq_along(pages), function(i) {
    sprintf("%s\n%s", pages[[i]], if (i < length(pages)) "\\pard\r\n\\sect" else "")
  }, character(1)), collapse = "")
  sprintf("{\\rtf1\\ansi\\deff0\r\n{\\fonttbl{\\f0 Times New Roman;}}\r\n%s\r\n}", page_blocks)
}


# FIXTURE 1: Single-parameter, single-timeline table
# Simple 4-column table, 1 page, no filtering needed.


make_rtf_simple <- function(path) {
  wl <- 2160L; wd <- 1440L  # label / data column widths in twips

  hdr <- rtf_row(list(
    cell("Measure",       wl, bold = TRUE, align = "l"),
    cell("Baseline",      wd, bold = TRUE),
    cell("2 min Escape",  wd, bold = TRUE),
    cell("10 min Escape", wd, bold = TRUE)
  ), trhdr = TRUE)

  rows <- paste(
    rtf_row(list(cell("Mean",  wl, align = "l"), cell("52",    wd), cell("185.3", wd), cell("24.1", wd))),
    rtf_row(list(cell("SD",    wl, align = "l"), cell("49",    wd), cell("178.6", wd), cell("22.7", wd))),
    rtf_row(list(cell("Range", wl, align = "l"), cell("51",    wd), cell("172.4", wd), cell("21.3", wd)))
  )

  page <- paste(
    rtf_sectd(),
    rtf_header_section(parameter = "Audience Noise (DB)"),
    rtf_footer_section("Page 1"),
    hdr, rows,
    sep = "\r\n"
  )

  writeLines(rtf_document(list(page)), path, useBytes = FALSE)
  invisible(path)
}

# ============================================================
# FIXTURE 2: Multi-parameter table (3 pages, one per parameter)
# Tests parameter filtering.
# ============================================================

make_rtf_multi_param <- function(path) {
  wl <- 2160L; wd <- 1440L

  hdr <- rtf_row(list(
    cell("Measure",       wl, bold = TRUE, align = "l"),
    cell("Baseline",      wd, bold = TRUE),
    cell("2 min Escape",  wd, bold = TRUE),
    cell("10 min Escape", wd, bold = TRUE)
  ), trhdr = TRUE)

  make_page <- function(param, page_n) {
    rows <- paste(
      rtf_row(list(cell("Mean",  wl, align = "l"), cell("52",    wd), cell("185.3", wd), cell("24.1", wd))),
      rtf_row(list(cell("SD",    wl, align = "l"), cell("49",    wd), cell("178.6", wd), cell("22.7", wd))),
      rtf_row(list(cell("Range", wl, align = "l"), cell("51",    wd), cell("172.4", wd), cell("21.3", wd)))
    )
    paste(
      rtf_sectd(),
      rtf_header_section(parameter = param),
      rtf_footer_section(sprintf("Page %d", page_n)),
      hdr, rows,
      sep = "\r\n"
    )
  }

  pages <- list(
    make_page("Audience Noise (DB)", 1L),
    make_page("No. Feinters (n)",    2L),
    make_page("Clap Density",        3L)
  )

  writeLines(rtf_document(pages), path, useBytes = FALSE)
  invisible(path)
}

# ============================================================
# FIXTURE 3: Multi-timeline table (timelines interleaved across 2 pages)
# Tests timeline filtering.
# ============================================================

make_rtf_multi_timeline <- function(path) {
  wl <- 2160L; wd <- 1440L

  hdr <- rtf_row(list(
    cell("Show Week",     wl, bold = TRUE, align = "l"),
    cell("Measure",       wl, bold = TRUE, align = "l"),
    cell("Baseline",      wd, bold = TRUE),
    cell("2 min Escape",  wd, bold = TRUE),
    cell("10 min Escape", wd, bold = TRUE)
  ), trhdr = TRUE)

  # Timeline label row: only col 1 filled, rest empty
  timeline_row <- function(label) {
    rtf_row(list(cell(label, wl, align = "l"), cell("", wl), cell("", wd), cell("", wd), cell("", wd)))
  }

  data_rows <- function() {
    paste(
      rtf_row(list(cell("", wl, align = "l"), cell("Mean", wl, align = "l"), cell("52", wd), cell(12.2, wd), cell("24.1", wd))),
      rtf_row(list(cell("", wl, align = "l"), cell("SD",   wl, align = "l"), cell("49", wd), cell(14.7,   wd), cell("22.7", wd)))
    )
  }

  page1 <- paste(
    rtf_sectd(),
    rtf_header_section(parameter = "Audience Noise (DB)"),
    rtf_footer_section("Page 1"),
    hdr,
    timeline_row("Week 1"),  data_rows(),
    timeline_row("Week 4"),  data_rows(),
    sep = "\r\n"
  )

  page2 <- paste(
    rtf_sectd(),
    rtf_header_section(parameter = "Audience Noise (DB)"),
    rtf_footer_section("Page 2"),
    hdr,
    timeline_row("Week 8"),  data_rows(),
    timeline_row("Week 12"), data_rows(),
    sep = "\r\n"
  )

  writeLines(rtf_document(list(page1, page2)), path, useBytes = FALSE)
  invisible(path)
}


# FIXTURE 4: Multi-parameter AND multi-timeline
# Tests both filters together.

make_rtf_combined_filters <- function(path) {
  wl <- 2160L; wd <- 1440L

  hdr <- rtf_row(list(
    cell("Show Week",     wl, bold = TRUE, align = "l"),
    cell("Measure",       wl, bold = TRUE, align = "l"),
    cell("Baseline",      wd, bold = TRUE),
    cell("2 min Escape",  wd, bold = TRUE),
    cell("10 min Escape", wd, bold = TRUE)
  ), trhdr = TRUE)

  # Timeline label row: only col 1 filled, rest empty
  timeline_row <- function(label) {
    rtf_row(list(cell(label, wl, align = "l"), cell("", wl), cell("", wd), cell("", wd), cell("", wd)))
  }

  data_rows <- function() {
    paste(
      rtf_row(list(cell("", wl, align = "l"), cell("Mean", wl, align = "l"), cell("52", wd), cell(12.2, wd), cell("24.1", wd))),
      rtf_row(list(cell("", wl, align = "l"), cell("SD",   wl, align = "l"), cell("49", wd), cell(14.7,   wd), cell("22.7", wd)))
    )
  }

  make_param_page <- function(param, page_n) {
    paste(
      rtf_sectd(),
      rtf_header_section(parameter = param),
      rtf_footer_section(sprintf("Page %d", page_n)),
      hdr,
      timeline_row("Week 1"),
      data_rows(),
      timeline_row("Week 4"),
      data_rows(),
      sep = "\n"
    )
  }

  pages <- list(
    make_param_page("Cholesterol",   1L),
    make_param_page("Triglycerides", 2L)
  )

  writeLines(rtf_document(pages), path, useBytes = FALSE)
  invisible(path)
}



# ============================================================
# FIXTURE 6: Image RTF (PNG embedded via \pngblip)
# Minimal valid PNG (1x1 white pixel), declared at 4x3 inches.
# ============================================================

make_rtf_image <- function(path) {
  # Smallest valid PNG: 1x1 white pixel, 67 bytes
  png_hex <- paste0(
    "89504e470d0a1a0a",                          # signature
    "0000000d49484452",                          # IHDR length + type
    "00000001",                                  # width = 1
    "00000001",                                  # height = 1
    "08020000009001",                            # bit depth, colour, compression, filter, interlace + CRC (partial)
    "2e00",                                      # CRC remainder
    "0000000c49444154",                          # IDAT length + type
    "789c6260f8cf00000002",                      # zlib-compressed pixel data
    "00019e21bc33",                              # IDAT CRC
    "00000000",                                  # IEND length
    "49454e44ae426082"                           # IEND type + CRC
  )

  w_twips <- 5760L  # 4 inches
  h_twips <- 4320L  # 3 inches

  pict <- sprintf(
    "{\\pict\\pngblip\\picwgoal%d\\pichgoal%d\n%s\n}",
    w_twips, h_twips, png_hex
  )

  page <- paste(
    rtf_sectd(),
    rtf_header_section(parameter = NULL),
    rtf_footer_section("Page 1"),
    pict,
    sep = "\n"
  )

  writeLines(rtf_document(list(page)), path, useBytes = FALSE)
  invisible(path)
}

# ============================================================
# FIXTURE 7: Escape sequences (\'xx hex, \uN unicode, \ucN fallbacks)
# CP1252 + Unicode decoding; all escapes are plain ASCII on disk.
# ============================================================

make_rtf_escapes <- function(path) {
  wl <- 2160L; wd <- 1440L

  hdr <- rtf_row(list(
    cell("Statistic",              wl, bold = TRUE, align = "l"),
    cell("Mean \\'b1 SD",          wd, bold = TRUE),   # plus-minus
    cell("\\'93Range\\'94",        wd, bold = TRUE)    # CP1252 smart quotes
  ), trhdr = TRUE)

  rows <- paste(
    rtf_row(list(cell("\\uc1\\u8804\\'3f 5 years", wl, align = "l"),   # <= with hex fallback
                 cell("185.3 \\'b1 24.1", wd),
                 cell("120\\u8211?190", wd))),             # en dash, ? fallback
    rtf_row(list(cell("\\uc0\\u916 from baseline", wl, align = "l"),   # Delta, no fallback
                 cell("-6.7 \\'b1 3.2", wd),
                 cell("-12\\u8211?-1", wd)))
  )

  page <- paste(
    rtf_sectd(),
    rtf_header_section(parameter = NULL),
    rtf_footer_section("Page 1"),
    hdr, rows,
    sep = "\n"
  )

  writeLines(rtf_document(list(page)), path, useBytes = FALSE)
  invisible(path)
}

# ============================================================
# FIXTURE 8: Spanning by cell width (no \clmgf / \clmrg)
# Some generators widen a single cell across several columns instead of
# emitting merge tags; the parser must normalise these to colspans.
# ============================================================

make_rtf_span_by_width <- function(path) {
  wl <- 2160L; wd <- 1440L

  # Top header row: "Dose A" physically covers the N + Mean columns
  # (width 2880) with no merge tags
  top_hdr <- rtf_row(list(
    cell("",       2L * wl, bold = TRUE, align = "l"),
    cell("Esacpology", 2L * wd, bold = TRUE),
    cell("Illusions", 2L * wd, bold = TRUE)
  ), trhdr = TRUE)

  sub_hdr <- rtf_row(list(
    cell("Show Week",     wl, bold = TRUE, align = "l"),
    cell("Measure",   wl, bold = TRUE, align = "l"),
    cell("2 min escape",   wd, bold = TRUE),
    cell("10 min escape", wd, bold = TRUE),
    cell("2 min escape",   wd, bold = TRUE),
    cell("10 min escape", wd, bold = TRUE)
  ), trhdr = TRUE)

  # Timeline label as one full-width cell (2160 + 3 * 1440 = 6480 twips)
  label_row <- rtf_row(list(cell("Week 1", 2L * wl + 4L * wd, align = "l")))

  rows <- paste(
    label_row,
    rtf_row(list(cell("", wl, align = "l"), cell("Mean", wl, align = "l"), cell("52", wd), cell(12.2, wd), cell("24.1", wd), cell("242.1", wd))),
    rtf_row(list(cell("", wl, align = "l"), cell("SD",   wl, align = "l"), cell("49", wd), cell(14.7, wd), cell("22.7", wd), cell("242.1", wd)))
  )

  page <- paste(
    rtf_sectd(),
    rtf_header_section(parameter = NULL),
    rtf_footer_section("Page 1"),
    top_hdr, sub_hdr, rows,
    sep = "\n"
  )

  writeLines(rtf_document(list(page)), path, useBytes = FALSE)
  invisible(path)
}

# ============================================================
# MAIN: generate all fixtures
# ============================================================

#' Generate all test RTF fixtures into a directory
#'
#' @param output_dir Directory to write the RTF files into (created if needed)
make_test_rtfs <- function(output_dir = "tests/testthat/fixtures") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  files <- list(
    "simple.rtf"            = make_rtf_simple,
    "multi_param.rtf"       = make_rtf_multi_param,
    "multi_timeline.rtf"    = make_rtf_multi_timeline,
    "combined_filters.rtf"  = make_rtf_combined_filters,
    "merged_headers.rtf"    = make_rtf_span_by_width,
    "image.rtf"             = make_rtf_image,
    "escapes.rtf"           = make_rtf_escapes
  )

  for (fname in names(files)) {
    fpath <- file.path(output_dir, fname)
    files[[fname]](fpath)
    message(sprintf("  Written: %s", fpath))
  }

  message(sprintf("\n%d RTF fixtures written to: %s", length(files), output_dir))
  invisible(output_dir)
}
