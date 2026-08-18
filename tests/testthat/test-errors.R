# Structured error taxonomy: every user-reachable failure should surface as a
# houdini_error carrying an actionable hint, not a raw internal condition.

expect_houdini <- function(expr, kind) {
  e <- tryCatch(expr, error = function(e) e)
  expect_s3_class(e, "houdini_error")
  expect_s3_class(e, paste0("houdini_error_", kind))
  # A hint is the whole point of the taxonomy
  expect_true(!is.null(err_hint(e)) && nzchar(err_hint(e)))
  invisible(e)
}

test_that("unreadable RTF paths raise houdini errors", {
  expect_houdini(parse_rtf("definitely-not-here.rtf"), "rtf_unreadable")
  expect_houdini(parse_rtf(tempdir()), "rtf_unreadable")
})

test_that("an RTF with no table sections reports a parse failure", {
  p <- tempfile(fileext = ".rtf")
  writeLines("this is not valid rtf at all", p)
  on.exit(unlink(p), add = TRUE)

  e <- expect_houdini(parse_rtf(p), "rtf_parse_failed")
  expect_match(conditionMessage(e), "sectd", fixed = TRUE)
  # The failing file is named, so a batch log points at the right row
  expect_match(conditionMessage(e), basename(p), fixed = TRUE)
})

test_that("unreadable .docx files raise houdini errors", {
  expect_houdini(open_docx("definitely-not-here.docx"), "docx_unreadable")

  fake <- tempfile(fileext = ".docx")
  writeLines("I am not a zip", fake)
  on.exit(unlink(fake), add = TRUE)
  e <- expect_houdini(open_docx(fake), "docx_unreadable")
  expect_match(conditionMessage(e), "document.xml", fixed = TRUE)
})

test_that("unreadable .xlsx files raise houdini errors", {
  expect_houdini(read_xlsx("definitely-not-here.xlsx"), "excel_unreadable")

  bad <- tempfile(fileext = ".xlsx")
  writeLines("not really a workbook", bad)
  on.exit(unlink(bad), add = TRUE)
  expect_houdini(read_xlsx(bad), "excel_unreadable")
})

test_that("apparate input validation uses the taxonomy", {
  docx <- make_min_docx()
  on.exit(unlink(docx), add = TRUE)

  expect_houdini(apparate("nope.docx", data.frame(), tempdir()), "docx_unreadable")
  expect_houdini(apparate(docx, data.frame(), "no-such-dir"), "rtf_folder_missing")
})

test_that("as_houdini_error passes through and wraps appropriately", {
  # Already structured: returned untouched, keeping its specific hint
  orig <- err_bookmark_missing("BM")
  expect_identical(as_houdini_error(orig, err_xml_inject_failed, "BM"), orig)

  # Bare condition: wrapped so it gains a message and hint
  bare <- simpleError("subscript out of bounds")
  wrapped <- as_houdini_error(bare, err_xml_inject_failed, "BM")
  expect_s3_class(wrapped, "houdini_error_xml_inject_failed")
  expect_match(conditionMessage(wrapped), "BM", fixed = TRUE)
  expect_match(conditionMessage(wrapped), "subscript out of bounds", fixed = TRUE)
  expect_true(nzchar(err_hint(wrapped)))
})

test_that("per-row failures are structured and do not abort the run", {
  docx    <- make_min_docx(bookmarks = c("TableBM", "Other"))
  rtf_dir <- tempfile()
  dir.create(rtf_dir)
  on.exit(unlink(c(docx, rtf_dir), recursive = TRUE), add = TRUE)

  writeLines("not valid rtf", file.path(rtf_dir, "broken.rtf"))
  file.copy(test_path("fixtures", "simple.rtf"), file.path(rtf_dir, "good.rtf"))

  config <- data.frame(
    Bookmark = c("TableBM", "Other", "NoSuchBookmark"),
    Dataset  = c("broken.rtf", "good.rtf", "good.rtf"),
    stringsAsFactors = FALSE
  )
  status <- apparate(docx, config, rtf_dir, quiet = TRUE)
  on.exit(unlink(paste0(docx, "_Houdini_Output.docx")), add = TRUE)

  # A corrupt RTF is attributed to the parse, not to XML generation
  expect_s3_class(status[["1"]]$err, "houdini_error_rtf_parse_failed")
  # A healthy row still succeeds despite its neighbours failing
  expect_null(status[["2"]]$err)
  # A missing bookmark keeps its own specific error
  expect_s3_class(status[["3"]]$err, "houdini_error_bookmark_missing")

  # Every failure carries a hint the log can print
  for (k in c("1", "3")) {
    expect_true(nzchar(err_hint(status[[k]]$err)))
  }
})

test_that("the generation log records causes and hints", {
  config <- data.frame(Bookmark = "BM", Table = "tbl", stringsAsFactors = FALSE)
  status <- list("1" = list(err = err_rtf_parse_failed("x/broken.rtf", "split_pages",
                                                       "no sections")))
  lines <- write_log("doc.docx", "cfg.xlsx", config, list(), status, "rtfs")
  txt   <- paste(lines, collapse = "\n")

  expect_match(txt, "ERROR - Failed to parse RTF file 'broken.rtf'", fixed = TRUE)
  expect_match(txt, "Hint      :", fixed = TRUE)
  # A data.frame config must not spill one "Excel File" line per column
  expect_length(grep("^Excel File", lines), 1L)
})

test_that("the log names tables without duplicating the extension", {
  config <- data.frame(Bookmark = c("A", "B"),
                       Table    = c("plain", "already.rtf"),
                       stringsAsFactors = FALSE)
  lines <- write_log("d", "c", config, list(), list(), "rtfs")
  txt   <- paste(lines, collapse = "\n")

  expect_match(txt, "Table     : plain.rtf", fixed = TRUE)
  expect_match(txt, "Table     : already.rtf", fixed = TRUE)
  expect_false(grepl(".rtf.rtf", txt, fixed = TRUE))
})

test_that("write_log reports a data.frame config source readably", {
  config <- data.frame(Bookmark = "BM", Table = "tbl", stringsAsFactors = FALSE)
  lines  <- write_log("doc.docx", config, config, list(), list(), "rtfs")
  expect_length(grep("^Excel File", lines), 1L)
  expect_match(paste(lines, collapse = "\n"), "(config data.frame)", fixed = TRUE)
})

test_that("empty parse results do not crash the table pipeline", {
  # A block with no rows or columns must flow through untouched rather than
  # indexing column 1 of a 0 x 0 matrix
  empty <- block_new()


  combined <- list(header = block_new(), data = block_new(),
                   footer = block_new(), col_widths_twips = numeric())
  expect_identical(remove_continuations(combined), combined)
  expect_identical(remove_double_blanks(combined), combined)
})
