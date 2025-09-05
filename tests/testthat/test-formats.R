test_that("standard_format() works",{
  data <- create_example_df_nonrandom()

  expected_data <- data.frame(
    ROWLBL1 = c(NA,"Baseline","","","",NA,"Week 1","","","",NA,"Week 2","","","",NA,"Week 3","","",""),
    BUFFER1 = rep(NA,20),
    ROWLBL2 = c(NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD",NA,"Observed","n","Mean","SD"),
    BUFFER2 = rep(NA,20),
    COL1 = c(NA,1:4,NA,5:8,NA,9:12,NA,13:16),
    COL2 = c(NA,17:20,NA,21:24,NA,25:28,NA,29:32),
    BUFFER3 = rep(NA,20),
    COL3 = c(NA,33:36,NA,37:40,NA,41:44,NA,45:48),
    COL4 = c(NA,49:52,NA,53:56,NA,57:60,NA,61:64),
    BUFFER4 = rep(NA,20),
    COL5 = c(NA,65:68,NA,69:72,NA,73:76,NA,77:80),
    PAGE = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4))
  expected_labels <- list("Visit","   ","","   ","Part A^*^Placebo","Part A^*^Treatment","   ","Part B^*^Placebo","Part B^*^Treatment","   ","Total")

  expected_data <- expected_data %>%
    replace_labels(expected_labels, add = TRUE)

  expected_ht <- houdinitable(expected_data, names(expected_data)[-ncol(expected_data)])
  expected_ht <- expected_ht %>%
    set_alignments("center",c("COL1","COL2","COL3","COL4","COL5")) %>%
    set_alignments("start",c("ROWLBL1","ROWLBL2")) %>%
    paginate(expected_data[["PAGE"]])

  expect_equal(standard_format(data), expected_ht)

  filters <- list(paramters = c(), timelines = c("Baseline","Week 3"))

  expected_data <- expected_data %>%
    timeline_filtering(filters$timelines)

  expected_ht <- houdinitable(expected_data, names(expected_data)[-ncol(expected_data)])
  expected_ht <- expected_ht %>%
    set_alignments("center",c("COL1","COL2","COL3","COL4","COL5")) %>%
    set_alignments("start",c("ROWLBL1","ROWLBL2")) %>%
    paginate(expected_data[["PAGE"]])

  expect_equal(standard_format(data, filters = filters), expected_ht)

})


test_that("non_standard_format() works",{
  data <- create_non_standard_df_nonrandom()
  expected_data <- data.frame(
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"),
    COL1 = seq(1,76,5),
    COL2 = seq(2,77,5),
    COL3 = seq(3,78,5),
    COL4 = seq(4,79,5),
    COL5 = seq(5,80,5))
  expected_labels <- list("Visit","","Part A^*^Placebo","Part A^*^Treatment","Part B^*^Placebo","Part B^*^Treatment","Total")
  expected_data <- expected_data %>%
    replace_labels(expected_labels, add = TRUE)
  expect_equal(non_standard_format(data),expected_data)
})
