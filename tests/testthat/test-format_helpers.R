#still to test
#-param filter
#add footer :()


test_that("Order Cols Works",{
  data <- create_example_df_nonrandom()
  expected_data <- data.frame(
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"),
    COL1 = c(1:16),
    COL2 = c(17:32),
    COL3 = c(33:48),
    COL4 = c(49:64),
    COL5 = c(65:80))
  expect_equal(order_cols(data), expected_data, ignore_attr = TRUE)
})

test_that("Separate Data Works",{
  expect_equal(separate_data(c("A","A","A","a","a","a")),c("A","","","a","",""))
})

test_that("Add row buffers Works",{
  data <- create_example_df_nonrandom() %>%
    order_cols()
  expected_data <- data.frame(
    ROWLBL1 = c(NA,"Baseline","Baseline","Baseline","Baseline",NA,"Week 1","Week 1","Week 1","Week 1",NA,"Week 2","Week 2","Week 2","Week 2",NA,"Week 3","Week 3","Week 3","Week 3"),
    ROWLBL2 = c(NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD"),
    COL1 = c(NA,1:4,NA,5:8,NA,9:12,NA,13:16),
    COL2 = c(NA,17:20,NA,21:24,NA,25:28,NA,29:32),
    COL3 = c(NA,33:36,NA,37:40,NA,41:44,NA,45:48),
    COL4 = c(NA,49:52,NA,53:56,NA,57:60,NA,61:64),
    COL5 = c(NA,65:68,NA,69:72,NA,73:76,NA,77:80))
  expect_equal(add_row_buffers(data),expected_data, ignore_attr = TRUE)
})

test_that("Add column buffers Works",{
  data <- create_example_df_nonrandom() %>%
    order_cols()
  expected_data <- data.frame(
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    BUFFER1 = rep(NA,16),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"),
    BUFFER2 = rep(NA,16),
    COL1 = c(1:16),
    COL2 = c(17:32),
    BUFFER3 = rep(NA,16),
    COL3 = c(33:48),
    COL4 = c(49:64),
    BUFFER4 = rep(NA,16),
    COL5 = c(65:80))
  expect_equal(add_col_buffers(data),expected_data, ignore_attr = TRUE)
})

test_that("Add Page Column Works",{
  data <- create_example_df() %>%
    order_cols() %>%
    add_row_buffers()
  PAGE <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
  expected_data <- add_column(data,PAGE)
  expect_equal(add_page_column(data),expected_data,ignore_attr = TRUE)
})



test_that("Timeline Filtering Works",{
  data <- create_example_df_nonrandom() %>%
    order_cols()
  data[[1]] <- data[[1]] %>%
    separate_data()
  data <- data %>%
    add_row_buffers() %>%
    add_page_column()
  expected_data <- data.frame(
    ROWLBL1 = c(NA,"Baseline","","","",NA,"Week 3","","",""),
    ROWLBL2 = c(NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD"),
    COL1 = c(NA,1:4,NA,13:16),
    COL2 = c(NA,17:20,NA,29:32),
    COL3 = c(NA,33:36,NA,45:48),
    COL4 = c(NA,49:52,NA,61:64),
    COL5 = c(NA,65:68,NA,77:80),
    PAGE = c(1,1,1,1,1,4,4,4,4,4))
  expect_equal(timeline_filtering(data,c("Week 3", "Baseline")),expected_data, ignore_attr = TRUE)
  expect_equal(timeline_filtering(data,c("Week 3", "Baseline","NOTAFILTER")),expected_data, ignore_attr = TRUE)
})
