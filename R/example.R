
create_example_df_nonrandom <- function(){
  labels <- c("Part B^*^Placebo","Total","Visit","Part A^*^Treatment","Part B^*^Treatment","Part A^*^Placebo","")
  data <- data.frame(
    COL3 = c(33:48),
    COL5 = c(65:80),
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    COL2 = c(17:32),
    COL4 = c(49:64),
    COL1 = c(1:16),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"))

  data <- data %>% replace_labels(labels, add = TRUE)
  data
}

create_example_trt_df_nonrandom <- function(){
  labels <- c("Placebo","Total","Visit","Treatment","Treatment","Placebo","")
  data <- data.frame(
    COL3 = c(33:48),
    COL5 = c(65:80),
    ROWLBL1 = c("Baseline","Baseline","Baseline","Baseline","Week 1","Week 1","Week 1","Week 1","Week 2","Week 2","Week 2","Week 2","Week 3","Week 3","Week 3","Week 3"),
    COL2 = c(17:32),
    COL4 = c(49:64),
    COL1 = c(1:16),
    ROWLBL2 = c("Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD","Observed","n","Mean","SD"),
    TRTLBL12 = rep("Part A",16),
    TRTLBL34 = rep("Part B",16))


  data <- data %>% replace_labels(labels, add = TRUE)
  data
}

create_example_df <- function(){
  labels <- c("Part B^*^Placebo","Total","Visit","Part A^*^Treatment","Part B^*^Treatment","Part A^*^Placebo","")
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
    ROWLBL1 = c(rep("Baseline", 20),rep("Week 1", 20),rep("Week 2", 20),rep("Week 3", 20)),
    ROWLBL2 = rep(c(rep("Observed", 5),rep("n", 5),rep("Mean", 5),rep("SD", 5)),4),
    CELLVALC1 = sample(1:100, 80, replace = TRUE),
    COLVAR1 = c(rep(c("Part A","Part A","Part B", "Part B",""),16)),
    COLVAR2 = c(rep(c("Placebo","Treatment","Placebo","Treatment","Total"),16))
    )
  labels <- c("Visit","","","","")
  data <- data %>% replace_labels(labels, add = TRUE)
  data
}

create_non_standard_df_nonrandom <- function(){
  data <- data.frame(
    ROWLBL1 = c(rep("Baseline", 20),rep("Week 1", 20),rep("Week 2", 20),rep("Week 3", 20)),
    ROWLBL2 = rep(c(rep("Observed", 5),rep("n", 5),rep("Mean", 5),rep("SD", 5)),4),
    CELLVALC1 = c(1:80),
    COLVAR1 = c(rep(c("Part A","Part A","Part B", "Part B",""),16)),
    COLVAR2 = c(rep(c("Placebo","Treatment","Placebo","Treatment","Total"),16))
  )
  labels <- c("Visit","","","","")
  data <- data %>% replace_labels(labels, add = TRUE)
  data
}


iss_example <- function(){
  data <- data.frame(
    ROWLBL1= c(rep("Completed Study",6), rep("Discontinued Study",6), rep("Primary Reason for discontinuation",6),rep(" Adverse Event",6), rep(" Death",6), rep(" Pregnancy",6),rep(" Protocol deviation",6), rep(" Withdrawal of consent",6),rep(" Study terminated by sponsor",6), rep(" Lost to follow up",6),rep(" Other",6)),
    COLVAR3 = rep(rep(c("Placebo","Treatment"),3),11),
    COLVAR2 = rep(c("HS-301","HS-301","HS-302","HS-302","Total","Total"),11),
    COLVAR1 = rep(rep(c("Phase 3 Placebo-Controlled Data at Target Dose"),6),11),
    CELLVALC2 = c(1:66))
  labels <- c("Disposition","","","","")
  data <- data %>% replace_labels(labels, add = TRUE)
  data

}
