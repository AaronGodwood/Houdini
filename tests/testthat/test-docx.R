# Word document handling: bookmark extraction, table/image injection,
# and the end-to-end process_document() pipeline.
# All tests build a minimal .docx from scratch (see helper-docx.R).

test_that("extract_bookmarks lists names and skips internal bookmarks", {
  docx <- make_min_docx(bookmarks = c("TableBM", "ImageBM", "_GoBack"))
  on.exit(unlink(docx), add = TRUE)

  bm <- extract_bookmarks(docx)
  expect_identical(sort(names(bm)), c("ImageBM", "TableBM"))
})

test_that("open_docx reads text width and builds the bookmark jump table", {
  docx <- make_min_docx()
  on.exit(unlink(docx), add = TRUE)

  session <- open_docx(docx)
  on.exit(unlink(session$tmp_dir, recursive = TRUE), add = TRUE)

  # 12240 - 1800 - 1800 = 8640 twips of text width
  expect_identical(session$text_width_emu, 8640L * 635L)
  expect_identical(sort(names(session$jump_table)), c("ImageBM", "TableBM"))
  expect_identical(session$next_img_id, 1L)
})

test_that("next image id skips past non-contiguous media names", {
  docx <- make_min_docx(media = list(
    "image1.png" = as.raw(1:4),
    "image3.png" = as.raw(1:4)
  ))
  on.exit(unlink(docx), add = TRUE)

  session <- open_docx(docx)
  on.exit(unlink(session$tmp_dir, recursive = TRUE), add = TRUE)
  expect_identical(session$next_img_id, 4L)
})

test_that("inject_table places a table after the bookmark paragraph", {
  docx <- make_min_docx()
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(c(docx, out)), add = TRUE)

  session <- open_docx(docx)
  xml_str <- get_table_xml(test_path("fixtures", "simple.rtf"),
                           text_width_twips = 8640L)$xml
  inject_table(session, "TableBM", xml_str)
  close_docx(session, out)

  doc <- read_docx_document(out)
  expect_match(doc, "<w:tbl", fixed = TRUE)
  expect_match(doc, "Measure", fixed = TRUE)
  # Injected table sits after the bookmark paragraph
  expect_true(regexpr("TableBM", doc, fixed = TRUE) <
                regexpr("<w:tbl", doc, fixed = TRUE))
})

test_that("inject_table errors with a houdini error for unknown bookmarks", {
  docx <- make_min_docx()
  on.exit(unlink(docx), add = TRUE)
  session <- open_docx(docx)
  on.exit(unlink(session$tmp_dir, recursive = TRUE), add = TRUE)

  expect_error(inject_table(session, "Nope", "<w:tbl></w:tbl>"),
               class = "houdini_error_bookmark_missing")
})

test_that("extract_bookmarks reports a table-cell bookmark as bad context", {
  docx <- make_min_docx(bookmarks = c("BodyBM", "CellBM"),
                        table_bookmarks = "CellBM")
  on.exit(unlink(docx), add = TRUE)

  bm  <- extract_bookmarks(docx)
  ctx <- bookmarks_contexts(bm)
  expect_identical(ctx[["BodyBM"]], "body")
  expect_identical(ctx[["CellBM"]], "table")
})

test_that("inject_table errors for a bookmark inside a table cell", {
  docx <- make_min_docx(bookmarks = c("TableBM"),
                        table_bookmarks = "TableBM")
  on.exit(unlink(docx), add = TRUE)
  session <- open_docx(docx)
  on.exit(unlink(session$tmp_dir, recursive = TRUE), add = TRUE)

  expect_error(inject_table(session, "TableBM", "<w:tbl></w:tbl>"),
               class = "houdini_error_bookmark_bad_context")
})

test_that("inject_image writes media, relationship and content type", {
  docx <- make_min_docx()
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(c(docx, out)), add = TRUE)

  img <- extract_png(test_path("fixtures", "image.rtf"))
  session <- open_docx(docx)
  inject_image(session, "ImageBM", img$png_bytes,
               img$width_twips, img$height_twips)
  close_docx(session, out)

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  unzip(out, exdir = tmp)

  expect_true(file.exists(file.path(tmp, "word", "media", "image1.png")))
  rels <- paste(readLines(file.path(tmp, "word", "_rels", "document.xml.rels"),
                          warn = FALSE), collapse = "")
  expect_match(rels, 'Id="rIdImg1"', fixed = TRUE)
  ct <- paste(readLines(file.path(tmp, "[Content_Types].xml"), warn = FALSE),
              collapse = "")
  expect_match(ct, 'Extension="png"', fixed = TRUE)

  doc <- paste(readLines(file.path(tmp, "word", "document.xml"), warn = FALSE),
               collapse = "\n")
  expect_match(doc, "<w:drawing>", fixed = TRUE)
  # Scaled to text width preserving 2:1 aspect (5760x4320 -> h = w * 0.75)
  w_emu <- 8640L * 635L
  expect_match(doc, sprintf('cx="%d"', w_emu), fixed = TRUE)
  expect_match(doc, sprintf('cy="%d"', round(w_emu * 4320 / 5760)), fixed = TRUE)
})

test_that("process_document injects everything and reports per-row status", {
  docx <- make_min_docx(bookmarks = c("TableBM", "ImageBM"))
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(c(docx, out)), add = TRUE)

  config <- data.frame(
    Bookmark = c("TableBM", "ImageBM"),
    Table    = c("tbl", "img"),
    stringsAsFactors = FALSE
  )
  rtf_paths <- list(
    tbl = test_path("fixtures", "simple.rtf"),
    img = test_path("fixtures", "image.rtf")
  )
  selections <- list("1" = list(excluded_cols = 2L))

  status <- process_document(docx, config, rtf_paths, selections, out)

  expect_true(all(vapply(status, function(s) is.null(s$err), logical(1))))
  expect_true(file.exists(out))

  doc <- read_docx_document(out)
  expect_match(doc, "<w:tbl", fixed = TRUE)
  expect_match(doc, "<w:drawing>", fixed = TRUE)
  # Excluded column 2 ("N") must not appear in the injected table
  expect_false(grepl(">N</w:t>", doc, fixed = TRUE))
})

test_that("re-running injection replaces content instead of duplicating it", {
  docx <- make_min_docx(bookmarks = c("TableBM", "ImageBM"))
  out1 <- tempfile(fileext = ".docx")
  out2 <- tempfile(fileext = ".docx")
  on.exit(unlink(c(docx, out1, out2)), add = TRUE)

  config <- data.frame(
    Bookmark = c("TableBM", "ImageBM"),
    Table    = c("tbl", "img"),
    stringsAsFactors = FALSE
  )
  rtf_paths <- list(
    tbl = test_path("fixtures", "simple.rtf"),
    img = test_path("fixtures", "image.rtf")
  )

  # First run into the pristine template, second run into the first output
  process_document(docx, config, rtf_paths, list(), out1)
  status <- process_document(out1, config, rtf_paths,
                             list("1" = list(excluded_cols = 2L)), out2)
  expect_true(all(vapply(status, function(s) is.null(s$err), logical(1))))

  doc <- read_docx_document(out2)
  # Exactly one table and one drawing, wrapped in the hidden markers
  # (the injected fragment keeps its xmlns attribute, so match "<w:tbl ")
  expect_identical(lengths(regmatches(doc, gregexpr("<w:tbl[ >]", doc))), 1L)
  expect_identical(lengths(regmatches(doc, gregexpr("<w:drawing>", doc))), 1L)

  # Second run's exclusion applied: column "N" gone from the replaced table
  expect_false(grepl(">N</w:t>", doc, fixed = TRUE))

  # Replaced image cleaned up: only the second run's media file remains
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  unzip(out2, exdir = tmp)
  media <- list.files(file.path(tmp, "word", "media"))
  expect_identical(length(media), 1L)
  rels <- paste(readLines(file.path(tmp, "word", "_rels", "document.xml.rels"),
                          warn = FALSE), collapse = "")
  expect_identical(lengths(regmatches(rels, gregexpr("rIdImg", rels))), 1L)
})

test_that("markers are hidden from extract_bookmarks", {
  docx <- make_min_docx(bookmarks = "TableBM")
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(c(docx, out)), add = TRUE)

  session <- open_docx(docx)
  xml_str <- get_table_xml(test_path("fixtures", "simple.rtf"))$xml
  inject_table(session, "TableBM", xml_str)
  close_docx(session, out)

  bm <- extract_bookmarks(out)
  expect_identical(names(bm), "TableBM")
})

test_that("process_document records errors instead of aborting", {
  docx <- make_min_docx(bookmarks = "TableBM")
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(c(docx, out)), add = TRUE)

  config <- data.frame(
    Bookmark = c("TableBM", "MissingBM"),
    Table    = c("nope", "tbl"),
    stringsAsFactors = FALSE
  )
  rtf_paths <- list(tbl = test_path("fixtures", "simple.rtf"))

  status <- process_document(docx, config, rtf_paths, list(), out)

  expect_s3_class(status[["1"]]$err, "houdini_error_rtf_unreadable")
  expect_s3_class(status[["2"]]$err, "houdini_error_bookmark_missing")
  expect_true(file.exists(out))
})
