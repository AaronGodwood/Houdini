

get_table_names <- function(path){
  table_names <- readxl::read_excel(path, sheet = 1) %>%
    dplyr::select(starts_with(c("Bookmark")))
}

prep_reference <- function(input){
  reference <- readxl::read_excel(input, sheet = 3) %>%
  dplyr::select(starts_with(c("Table Number", "TFL ID"))) %>%
    dplyr::mutate(across(c(1), ~ gsub("\\.","",.))) %>%
    dplyr::mutate(across(c(1), ~ sprintf("Table%s",.))) %>%
    dplyr::mutate(across(c(2), ~ sprintf("%s.sas7bdat",.))) %>%

    names(reference) <- c("Bookmark","Dataset")
}

generate_table <- function(table_names,reference){
  requested_tables <- tibble(
    Bookmark = character(),
    Dataset = character(),
    Format = character(),
    Landscape = character(),
    Header = character(),
    Notes = character()
  )
  requested_tables_list <- mapply(function(bookmark,dataset){
    pattern <- sprintf("^%s[a-z]?$",bookmark)
    if(any(sapply(table_names, function(x){grepl(pattern,x)}))){
      tibble(Bookmark = bookmark,Dataset = dataset)
    }
    else
      NULL

  },reference$Bookmark,reference$Dataset, SIMPLIFY = FALSE)

  requested_tables <- reference %>%
    filter(sapply(Bookmark, function(x) any(grepl( sprintf("^%s[a-z]?$", x ), table_names$Bookmark))))





}

input <- "M1095-HS-301_Non-Efficacy Dry Run_Delivery Notes and Comments Tracker.xlsx"
path <- "VELA-1 CSR Dry run test.xlsx"
