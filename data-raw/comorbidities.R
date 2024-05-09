## code to prepare `comorbidities` dataset goes here
comorbidities <- as.data.frame(
  readxl::read_xlsx(
    file.path("./data-raw", "Manuscript_Generic_Example.xlsx"),
    sheet = 2,
    col_names = TRUE,
    na = c("NA", "NaN", "", " ")
  )
)

comorbidities$Severity <- factor(comorbidities$Severity,
  levels = c("Mild", "Moderate", "Severe")
)

usethis::use_data(comorbidities, overwrite = TRUE)
