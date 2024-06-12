library(testthat)

#create sample data for cumulative excess plot from gensurv() function

df_risk <- gensurv(111, 2000, 1000, 36, 0.0032, 0.003, "Weeks") %>%
  mutate(outcome = "Risk")

df_ben <- gensurv(111, 2000, 1000, 36, 0.0035, 0.002, "Weeks") %>%
  mutate(outcome = "Benefit")

cumexcess_plot <- rbind(df_risk, df_ben) %>%
  mutate(eff_diff_lbl = "Active - Placebo")

# create sample data for table corresponding to the cumulative excess plot

cumexcess_table <- data.frame(
  effect = c("Active", "Active", "Active", "Active", "Active", "Active",
             "Active", "Active", "Active", "Active", "Active", "Active",
             "Active", "Active", "Placebo", "Placebo", "Placebo", "Placebo",
             "Placebo", "Placebo", "Placebo", "Placebo", "Placebo",
             "Placebo", "Placebo", "Placebo", "Placebo", "Placebo"),
  outcome = c("Benefit", "Benefit", "Benefit", "Benefit", "Benefit", "Benefit",
              "Benefit", "Risk", "Risk", "Risk", "Risk", "Risk",
              "Risk", "Risk", "Benefit", "Benefit", "Benefit", "Benefit",
              "Benefit", "Benefit", "Benefit", "Risk", "Risk", "Risk",
              "Risk", "Risk", "Risk", "Risk"),
  n = c(0, 50, 78, 117, 156, 194, 230, 0, 34, 71, 108, 156, 192, 232, 0, 7, 18,
        27, 43, 57, 70, 0, 13, 32, 45, 56, 69, 89)
) %>% mutate(eff_code = ifelse(effect == "Placebo", 0,
                               ifelse(effect == "Active", 1, 99))) %>%
  mutate(obsv_duration = 36)

# testing gensurv_table for ggplot object

test_that("gensurv_table() will ouput a ggplot object", {
  expect_true(inherits(gensurv_table(cumexcess_table, 100, 6), "ggplot")
  )
})

# testing df_table must have required variables

cumexcess_table1 <- cumexcess_table %>% select(effect, outcome,
                                               obsv_duration,
                                               eff_code)

test_that("gensurv_table() expects an error when df_table() is missing required
          variables", {
            expect_error(gensurv_table(cumexcess_table1, 100, 6)
            )
          })

cumexcess_table2 <- cumexcess_table %>% rename(results = outcome)

test_that("gensurv_table() expects an error when df_table() has misnamed
          variables", {
            expect_error(gensurv_table(cumexcess_table2, 100, 6))
          })

# testing gensurv_table's ability to handle missing data

cumexcess_table3 <- cumexcess_table
cumexcess_table3$outcome[1] <- NA

test_that("gensurv_table() will return a custom warning message concerning
missing data",
          {
            expect_warning(gensurv_table(cumexcess_table3, 100, 6))
          })

test_that("gensurv_table() will return a visible object with
missing data", {
            expect_visible(gensurv_table(cumexcess_table3, 100, 6)
            )
          })

# testing gensurv_plot for visible object

test_that("gensurv_plot() will ouput a visible object", {
  expect_visible(gensurv_plot(cumexcess_plot, 100, 6))
})

# testing df_outcome must have required variables

cumexcess_plot1 <- cumexcess_plot %>% select(eventtime, outcome,
                                             obsv_duration,
                                             eff_diff_lbl,
                                             obsv_unit)

test_that("gensurv_plot() expects an error when df_outcome() is missing required
          variables", {
            expect_error(gensurv_plot(cumexcess_plot1, 100, 6)
            )
          })

cumexcess_plot2 <- cumexcess_plot %>% rename(results = outcome)

test_that("gensurv_plot() expects an error when df_outcome() has misnamed
          variables", {
            expect_error(gensurv_plot(cumexcess_plot2, 100, 6))
          })

# testing gensurv_plot's ability to handle missing data

cumexcess_plot3 <- cumexcess_plot
cumexcess_plot3$outcome[1] <- NA

test_that("gensurv_plot() will return a custom warning concerning missing
          data", {
            expect_warning(gensurv_plot(cumexcess_plot3, 100, 6)
            )
          })

test_that("gensurv_plot() will return a visible object with missing
          data", {
            expect_visible(gensurv_plot(cumexcess_plot3, 100, 6)
            )
          })

# testing gensurv_combined outputs a visual object

test_that("gensurv_combined() will output a visible object", {
  expect_visible(
    gensurv_combined(
      df_plot = cumexcess_plot,
      subjects_pt = 100,
      visits_pt = 6,
      df_table = cumexcess_table
    )
  )
})

# testing gensurv_combined datasets must have required variables

test_that(
  "gensurv_combined() expects an error when df_table() and df_plot()
are missing required variables",
  {
    expect_error(
      gensurv_combined(
        df_plot = cumexcess_plot1,
        subjects_pt = 100,
        visits_pt = 6,
        df_table = cumexcess_table1
      )
    )
  }
)

test_that("gensurv_table() expects an error when df_table() and df_plot() have
misnamed variables",
          {
            expect_error(
              gensurv_combined(
                df_plot = cumexcess_plot2,
                subjects_pt = 100,
                visits_pt = 6,
                df_table = cumexcess_table2
              )
            )
          })


# testing gensurv_combined's ability to handle missing data

test_that("gensurv_combined() will return a custom warning concerning
          missing data",
          {
            expect_warning(
              gensurv_combined(
                df_plot = cumexcess_plot3,
                subjects_pt = 100,
                visits_pt = 6,
                df_table = cumexcess_table3
              )
            )
          })

test_that("gensurv_combined() will return a visible object with
          missing data",
          {
            expect_visible(
              gensurv_combined(
                df_plot = cumexcess_plot3,
                subjects_pt = 100,
                visits_pt = 6,
                df_table = cumexcess_table3
              )
            )
          })
