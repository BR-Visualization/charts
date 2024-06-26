## code to prepare `cumexcess` dataset goes here

load("~/brcharts/data-raw/manuscript_example.rda")

cumexcess <- manuscript_example %>%
  filter(subgroup == "Overall") %>%
  select(
    trt_diff_lbl, treat_code, treatment, study_duration, eventtimeunit,
    eventtime, smalln, outcome, trt_diff_f
  )

cumexcess <- cumexcess %>% rename(
  eff_diff_lbl = trt_diff_lbl,
  eff_code = treat_code, effect = treatment,
  obsv_duration = study_duration,
  obsv_unit = eventtimeunit, n = smalln,
  diff = trt_diff_f
)

cumexcess <- as.data.frame(cumexcess)

usethis::use_data(cumexcess, overwrite = TRUE)
