library(testthat)

# Create a sample data frame
demography <- data.frame(
  Type = c(1, 1, 2, 2),
  Gender = c("Females", "Males", "Females", "Males"),
  Age = c(20, 20, 30, 30),
  Prevalence = c(100, 200, 150, 250)
)

test_that("pyramid_chart returns a ggplot object", {
  result <- demography |>
    dplyr::mutate(
      Type = as.factor(paste0("Type ", Type)),
      figprev = ifelse(Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000),
      Sex = Gender
    ) |>
    pyramid_chart(
      levelvar = "Type", xvar = "figprev", yvar = "Age",
      groupvar = "Sex", alpha_set = 0.7, chartcolors = c("#FF0000", "#0000FF"),
      xlab = "Prevalence (x 100 000)"
    )

  expect_true(inherits(result, "ggplot"))
})

test_that("pyramid_chart handles missing data correctly", {
  demography_missing <- demography
  demography_missing$Prevalence[1] <- NA

  result <- demography_missing |>
    dplyr::mutate(
      Type = as.factor(paste0("Type ", Type)),
      figprev = ifelse(Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000),
      Sex = Gender
    ) |>
    pyramid_chart(
      levelvar = "Type", xvar = "figprev", yvar = "Age",
      groupvar = "Sex", alpha_set = 0.7, chartcolors = c("#FF0000", "#0000FF"),
      xlab = "Prevalence (x 100 000)"
    )

  expect_true(inherits(result, "ggplot"))
})

test_that("pyramid_chart handles different input types correctly", {
  # Test with numeric input
  result <- demography |>
    dplyr::mutate(
      figprev = ifelse(Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000),
      Sex = Gender
    ) |>
    pyramid_chart(
      levelvar = Type, xvar = "figprev", yvar = "Age",
      groupvar = "Sex", alpha_set = 0.7, chartcolors = c("#FF0000", "#0000FF"),
      xlab = "Prevalence (x 100 000)"
    )

  expect_true(inherits(result, "ggplot"))

  # Test with character input
  result <- demography |>
    dplyr::mutate(
      Type = as.character(Type),
      figprev = ifelse(Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000),
      Sex = Gender
    ) |>
    pyramid_chart(
      levelvar = "Type", xvar = "figprev", yvar = "Age",
      groupvar = "Sex", alpha_set = 0.7, chartcolors = c("#FF0000", "#0000FF"),
      xlab = "Prevalence (x 100 000)"
    )

  expect_true(inherits(result, "ggplot"))
})
