# Headless pipeline: config parsing and houdini_run().

test_that("parse_xl requires Bookmark and Dataset columns", {
  expect_error(
    parse_xl(data.frame(Foo = "a", Bar = "b")),
    "Excel file must contain 'Bookmark' and 'Dataset' columns"
  )
})

test_that("parse_xl reads filters and exclusions", {
  xl <- data.frame(
    Bookmark           = c("BM1", "BM2", ""),
    Dataset              = c("t1.rtf", "t2", ""),
    Parameters         = c("Chol; Gluc", NA, NA),
    ExcludedColumns    = c("2; 3", "", NA),
    ExcludedRows       = c(NA, "7", NA),
    stringsAsFactors   = FALSE
  )
  res <- parse_xl(xl)

  expect_identical(res$config$Table, c("t1", "t2", ""))
  expect_identical(res$selections[["1"]]$parameters, c("Chol", "Gluc"))
  expect_identical(res$selections[["1"]]$excluded_cols, c(2L, 3L))
  expect_null(res$selections[["1"]]$excluded_rows)
  expect_identical(res$selections[["2"]]$excluded_rows, 7L)
  expect_null(res$selections[["3"]])
})

test_that("apparate injects from a config workbook end to end", {
  docx    <- make_min_docx(bookmarks = c("TableBM", "ImageBM"))
  out     <- tempfile(fileext = ".docx")
  cfg_xl  <- tempfile(fileext = ".xlsx")
  rtf_dir <- tempfile()
  dir.create(rtf_dir)
  on.exit(unlink(c(docx, out, cfg_xl, rtf_dir), recursive = TRUE), add = TRUE)

  file.copy(test_path("fixtures", "simple.rtf"),
            file.path(rtf_dir, "tbl.rtf"))
  file.copy(test_path("fixtures", "image.rtf"),
            file.path(rtf_dir, "img.rtf"))

  writexl::write_xlsx(data.frame(
    Bookmark        = c("TableBM", "ImageBM"),
    Dataset         = c("tbl.rtf", "img.rtf"),
    ExcludedColumns = c("2", ""),
    stringsAsFactors = FALSE
  ), cfg_xl)

  status <- apparate(docx, cfg_xl, rtf_dir, quiet = TRUE)

  expect_true(all(vapply(status, function(s) is.null(s$err), logical(1))))
  expect_true(file.exists(paste0(docx,"_Houdini_Output.docx", collapse="")))

  doc <- read_docx_document(paste0(docx,"_Houdini_Output.docx", collapse=""))
  expect_match(doc, "<w:tbl[ >]")
  expect_match(doc, "<w:drawing>", fixed = TRUE)
  # ExcludedColumns applied: column 2 ("N") removed
  expect_false(grepl(">N</w:t>", doc, fixed = TRUE))
})

test_that("apparate accepts a data.frame config and reports row errors", {
  docx    <- make_min_docx(bookmarks = "TableBM")
  out     <- tempfile(fileext = ".docx")
  rtf_dir <- tempfile()
  dir.create(rtf_dir)
  on.exit(unlink(c(docx, out, rtf_dir), recursive = TRUE), add = TRUE)

  file.copy(test_path("fixtures", "simple.rtf"),
            file.path(rtf_dir, "tbl.rtf"))

  config <- data.frame(
    Bookmark = c("TableBM", "MissingBM"),
    Dataset    = c("tbl", "tbl"),
    stringsAsFactors = FALSE
  )
  status <- apparate(docx, config, rtf_dir, quiet = TRUE)

  expect_null(status[["1"]]$err)
  expect_s3_class(status[["2"]]$err, "houdini_error")
  expect_true(file.exists(paste0(docx,"_Houdini_Output.docx")))
})


test_that("absolute_path recognises platform-native absolute paths",{
  # Left alone: already absolute
  expect_identical(absolute_path("/tmp/doc.docx"), "/tmp/doc.docx")
  expect_identical(absolute_path("C:/Users/x/doc.docx"), "C:/Users/x/doc.docx")
  expect_identical(absolute_path("C:\\Users\\x\\doc.docx"), "C:\\Users\\x\\doc.docx")
  expect_identical(absolute_path("\\\\server\\share\\doc.docx"),
                   "\\\\server\\share\\doc.docx")

  # Resolved against the working directory: genuinely relative
  expect_identical(absolute_path("doc.docx"), file.path(getwd(), "doc.docx"))
  expect_identical(absolute_path("sub/doc.docx"), file.path(getwd(), "sub/doc.docx"))
})


test_that("houdini_run validates its inputs", {
  expect_error(apparate("nope.docx", data.frame(), tempdir()),
               "Word document not found: nope.docx")
  docx <- make_min_docx()
  on.exit(unlink(docx), add = TRUE)
  expect_error(apparate(docx, data.frame(), "no-such-dir"),
               "RTF folder not found: no-such-dir")
})



make_rtf_dirs <- function() {
  tdir <- withr::local_tempdir(.local_envir = parent.frame())
  fdir <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(test_path("fixtures", "multi_levels.rtf"), file.path(tdir, "t_01.rtf"))
  file.copy(test_path("fixtures", "image.rtf"), file.path(fdir, "f_01.rtf"))
  list(tables = tdir, figures = fdir)
}

test_that("collect_rtf_paths finds files in both folders", {
  d <- make_rtf_dirs()
  paths <- collect_rtf_paths(d$tables, d$figures)

  expect_setequal(names(paths), c("t_01", "f_01"))
  expect_match(paths[["f_01"]], basename(d$figures), fixed = TRUE)
})

test_that("a NULL or identical figure folder yields the table folder alone", {
  d <- make_rtf_dirs()

  expect_named(collect_rtf_paths(d$tables, NULL), "t_01")
  expect_named(collect_rtf_paths(d$tables, d$tables), "t_01")
})

test_that("a name in both folders resolves to the table folder, with a warning", {
  d <- make_rtf_dirs()
  file.copy(file.path(d$tables, "t_01.rtf"), file.path(d$figures, "t_01.rtf"))

  expect_warning(paths <- collect_rtf_paths(d$tables, d$figures),
                 "present in both RTF folders")
  expect_match(paths[["t_01"]], basename(d$tables), fixed = TRUE)
})

test_that("apparate injects a figure that lives outside the table folder", {
  d <- make_rtf_dirs()
  docx <- withr::local_tempfile(fileext = ".docx")
  make_min_docx(docx, bookmarks = c("BM1", "BM2"))
  cfg <- data.frame(Bookmark = c("BM1", "BM2"), Dataset = c("t_01", "f_01"),
                    stringsAsFactors = FALSE)

  # Without the figure folder the figure cannot be found at all
  without <- apparate(docx, cfg, d$tables, quiet = TRUE)
  expect_null(without[[1]]$err)
  expect_s3_class(without[[2]]$err, "houdini_error")

  with_figs <- apparate(docx, cfg, d$tables, figure_location = d$figures,
                        quiet = TRUE)
  expect_null(with_figs[[1]]$err)
  expect_null(with_figs[[2]]$err)

  doc <- xml2::read_xml(read_docx_document(paste0(docx, "_Houdini_Output.docx")))
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  expect_length(xml2::xml_find_all(doc, ".//w:tbl", ns = ns), 1)
  expect_length(xml2::xml_find_all(doc, ".//w:drawing", ns = ns), 1)
})

test_that("a figure_location that does not exist is reported, not ignored", {
  d <- make_rtf_dirs()
  docx <- withr::local_tempfile(fileext = ".docx")
  make_min_docx(docx, bookmarks = "BM1")
  cfg <- data.frame(Bookmark = "BM1", Dataset = "t_01", stringsAsFactors = FALSE)

  expect_error(
    apparate(docx, cfg, d$tables,
             figure_location = file.path(d$tables, "no-such-folder"),
             quiet = TRUE),
    "Figure folder not found"
  )
})

test_that("an empty figure_location falls back to the table folder", {
  d <- make_rtf_dirs()
  docx <- withr::local_tempfile(fileext = ".docx")
  make_min_docx(docx, bookmarks = "BM1")
  cfg <- data.frame(Bookmark = "BM1", Dataset = "t_01", stringsAsFactors = FALSE)

  status <- apparate(docx, cfg, d$tables, figure_location = "", quiet = TRUE)
  expect_null(status[[1]]$err)
})
