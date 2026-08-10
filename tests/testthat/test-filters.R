# Parameter/timeline filtering, page combining, and the shared column/row
# resolution helpers used by the HTML and XML renderers.

test_that("filter_pages keeps matching and NA-parameter pages", {
  pages <- parse_rtf(test_path("fixtures", "multi_param.rtf"))

  expect_length(filter_pages(pages, "Audience Noise (DB)")$pages, 1L)
  expect_length(filter_pages(pages, c("Audience Noise (DB)", "Clap Density"))$pages, 2L)
  # NULL / empty filter means keep everything
  expect_length(filter_pages(pages, NULL)$pages, 3L)
  expect_length(filter_pages(pages, character())$pages, 3L)

  # Pages without a parameter are always kept
  no_param <- parse_rtf(test_path("fixtures", "merged_headers.rtf"))
  expect_length(filter_pages(no_param, "Anything")$pages, 1L)
})


test_that("filter_timelines keeps only the selected blocks", {
  pages <- parse_rtf(test_path("fixtures", "multi_timeline.rtf"))
  combined <- combine_pages(pages)

  kept <- filter_timelines(combined, "Week 4")$combined
  # Page 1 holds Week 1 and Week 4; only the Week 4 block (label + 2 rows) stays
  expect_identical(block_nrow(kept$data), 3L)
  expect_identical(kept$data$text[1L, 1L], "Week 4")


  # No filter -> unchanged
  expect_identical(filter_timelines(combined, NULL)$combined, combined)
})

test_that("filter_levels keeps only selected indents", {
  pages <- parse_rtf(test_path("fixtures", "multi_levels.rtf"))
  combined <- combine_pages(pages)

  kept_level1 <- filter_levels(combined, "Trick Class")$combined
  # Expect to keep 2 level 1 rows and 1 blank row
  expect_identical(block_nrow(kept_level1$data), 3L)
  expect_identical(kept_level1$data$text[1L, 1L], "Illusions")

  kept_level2 <- filter_levels(combined, "Trick name/names")$combined
  # Expect to keep 4 level 2 rows and 1 blank row
  expect_identical(block_nrow(kept_level2$data), 5L)
  expect_identical(kept_level2$data$text[1L, 1L], "    Appirition")


  # No filter -> unchanged
  expect_identical(filter_levels(combined, NULL)$combined, combined)
})



test_that("combine_pages concatenates data and keeps first-page headers", {
  pages <- parse_rtf(test_path("fixtures", "multi_param.rtf"))
  combined <- combine_pages(pages)

  expect_identical(block_nrow(combined$header), 1L)
  expect_identical(block_nrow(combined$data), 9L)  # 3 pages x 2 rows
  expect_identical(combined$col_widths_twips, c(2160, 1440, 1440, 1440))

  empty <- combine_pages(list())
  expect_identical(block_nrow(empty$header), 0L)
  expect_identical(block_nrow(empty$data), 0L)
})

test_that("resolve_cols handles indices, names and out-of-range input",{
  pages    <- parse_rtf(test_path("fixtures", "simple.rtf"))
  combined <- combine_pages(pages)
  hdr      <- combined$header

  expect_identical(resolve_cols(NULL, 4L, hdr), 1:4)
  expect_identical(resolve_cols(c(1L, 3L), 4L, hdr), c(1L, 3L))
  expect_identical(resolve_cols(c("Baseline", "10 min Escape"), 4L, hdr), c(2L, 4L))

  # Out-of-range indices are dropped
  expect_identical(resolve_cols(c(2L, 99L), 4L, hdr), 2L)
  # Nothing valid -> all columns
  expect_identical(resolve_cols(99L, 4L, hdr), 1:4)
})

test_that("slice_range respects bounds", {
  expect_identical(slice_range(5L, 2, 4), 2:4)
  expect_identical(slice_range(5L, NULL, NULL), 1:5)
  expect_identical(slice_range(5L, 4, 2), integer())
  expect_identical(slice_range(0L, 1, 10), integer())
})

test_that("prepare_table applies exclusions", {
  prep <- prepare_table(
    path = test_path("fixtures", "simple.rtf"),
    excluded_cols = 2L,
    excluded_rows = c(1L, 3L)
  )
  expect_identical(prep$included_cols, c(1L, 3L, 4L))
  expect_identical(block_nrow(prep$combined$data), 1L)
  expect_identical(prep$combined$data$text[1L, 1L], "SD")
})

test_that("row exclusions are stable identities, not view positions", {
  pages <- parse_rtf(test_path("fixtures", "multi_timeline.rtf"))

  # IDs are sequential across pages in document order
  combined <- combine_pages(pages)
  expect_identical(combined$data$row_id,
                   seq_len(block_nrow(combined$data)))

  # Excluding the Week-4 Placebo row (ID 5) removes that same row even when
  # a timeline filter changes which rows are visible
  prep <- prepare_table(pages = pages, excluded_rows = 5L, timelines = "Week 4")
  expect_identical(prep$combined$data$text[, 3L], c("", "49"))  # label + Treatment A

  # An exclusion pointing at a row the filter already hides must not spill
  # over onto a different visible row
  prep2 <- prepare_table(pages = pages, excluded_rows = 2L, timelines = "Week 4")
  expect_identical(block_nrow(prep2$combined$data), 3L)

  # The selection pane emits the stable IDs so the app round-trips them
  html <- get_table_html_selection(
    test_path("fixtures", "multi_timeline.rtf"),
    timelines = "Week 4", pages = pages
  )
  expect_match(html, "data-row=\"4\" data-rowtype=\"data\"", fixed = TRUE)
  expect_match(html, "data-row=\"6\" data-rowtype=\"data\"", fixed = TRUE)
  expect_no_match(html, "data-row=\"1\" data-rowtype=\"data\"", fixed = TRUE)
})
