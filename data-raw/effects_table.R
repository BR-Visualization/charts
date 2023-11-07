## code to prepare `effects_table` dataset goes here
effects_table <- as.data.frame(
  readxl::read_xlsx(
    file.path("./data-raw", "Manuscript_Generic_Example.xlsx"),
    sheet = 1,
    col_names = TRUE,
    na = c("NA", "NaN", "", " ")
  )
)

effects_table[] <- lapply(effects_table, type.convert, as.is = TRUE)

usethis::use_data(effects_table, overwrite = TRUE)
