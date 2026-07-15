test_that("get_runs() works",{
  data <- c("A","A","A","A","B","B","B","C","C")
  expected_data <- c(4,0,0,0,3,0,0,2,0)
  expect_equal(get_runs(data),expected_data)
  expect_equal(get_runs(c("A")),c(1))
  expect_equal(get_runs(c()),c())
})

test_that("split_headers() works", {
  data <- create_example_df() %>%
    order_cols() %>%
    get_labels()
  expected_data <- data.frame(
    ROWLBL1 = c("", "Visit"),
    ROWLBL2 = c("", ""),
    COL1 = c("Part A","Placebo"),
    COL2 = c("Part A","Treatment"),
    COL3 = c("Part B","Placebo"),
    COL4 = c("Part B", "Treatment"),
    COL5 = c("","Total"))
  expect_equal(split_headers(data),expected_data)
})

