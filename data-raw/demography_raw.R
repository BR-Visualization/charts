## code to prepare `comorbidities` dataset goes here
demography <- read.csv("data-raw/demography_raw.csv")

usethis::use_data(demography, overwrite = TRUE)
