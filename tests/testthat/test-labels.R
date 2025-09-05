test_that("get_labels() works",{
  data <- create_example_df() %>%
    order_cols()
  expected_labels <- list("Visit","","Part A^*^Placebo","Part A^*^Treatment","Part B^*^Placebo","Part B^*^Treatment","Total")
  expect_equal(get_labels(data), expected_labels, ignore_attr = TRUE)
})

test_that("replace_labels() works",{
  data <- create_example_df_nonrandom() %>%
    order_cols()
  expected_data <- data.frame(
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"),
    COL1 = c(1:16),
    COL2 = c(17:32),
    COL3 = c(33:48),
    COL4 = c(49:64),
    COL5 = c(65:80))
  for(i in 1:ncol(expected_data)){
    attr(expected_data[[i]], "label") <- as.character(i)
  }
  new_labels <- c("1","2","3","4","5","6","7")
  expect_equal(replace_labels(data,new_labels),expected_data)
})
