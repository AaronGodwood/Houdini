test_that("apply_second_header() works",{
  data <- create_example_trt_df_nonrandom() %>%
    dplyr::select(starts_with(c( houdini_global$defaults$cols.name, houdini_global$defaults$rowlbls.name ))) %>%
    order_cols()
  header_code <- c("0","0","A","A","B","B","0")
  std_labels <- c(A = "Part A", B = "Part B")
  expected_data <- create_example_df_nonrandom() %>%
    order_cols()
  expect_equal(apply_second_header(data,std_labels,header_code),expected_data)
})


test_that("apply_trt_headers() works",{
  data <- create_example_trt_df_nonrandom() %>%
    apply_trt_headers() %>%
    dplyr::select(starts_with(c( houdini_global$defaults$cols.name, houdini_global$defaults$rowlbls.name ))) %>%
    order_cols()
  expected_data <- create_example_df_nonrandom() %>%
    order_cols()
  expect_equal(data,expected_data)
})


