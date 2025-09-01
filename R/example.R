


create_exmaple_df <- function(){
  labels <- c("Visit","","Part A^*^Placebo","Part A^*^Treatment","Part B^*^Placebo","Part B^*^Treatment", "Total")
  data <- data.frame(
    COL3 = sample(1:100, 16, replace = TRUE),
    COL5 = sample(1:100, 16, replace = TRUE),
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    COL2 = sample(1:100, 16, replace = TRUE),
    COL4 = sample(1:100, 16, replace = TRUE),
    COL1 = sample(1:100, 16, replace = TRUE),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"))

  data <- data %>% replace_labels(labels, add = TRUE)
  data
}

create_non_standard_df <- function(){
  data <- data.frame(
    ROWLBL1 = c(rep("Baseline", 20),rep("Week 1", 20),rep("Week 2", 20),rep("Week3", 20)),
    ROWLBL2 = rep(c(rep("Observed", 5),rep("n", 5),rep("Mean", 5),rep("SD", 5)),4),
    CELLVALC1 = sample(1:100, 80, replace = TRUE),
    COLVAR1 = c(rep(c("Part A","Part A","Part B", "Part B",""),16)),
    COLVAR2 = c(rep(c("Placebo","Treatment","Placebo","Treatment","Total"),16))
    )
  labels <- c("Visit","","","","")
  data <- data %>% replace_labels(labels, add = TRUE)
  data
}
