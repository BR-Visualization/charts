library(testthat)

# create sample data for scatterplot using rnorm()

set.seed(1234)
b1 <- rnorm(500, 248 / 500, 0.1)
b2 <- rnorm(500, 50 / 500, 0.1)
r1 <- rnorm(500, 44 / 500, 0.07)
r2 <- rnorm(500, 24 / 500, 0.07)
bdiff <- b1 - b2
rdiff <- r1 - r2
df_diff <- data.frame(bdiff, rdiff)
outcome <- c("Benefit", "Risk")

# testing scatter_plot for ggplot object

test_that("scatter_plot() will ouput a ggplot object", {
  expect_true(inherits(scatter_plot(df_diff, outcome), "ggplot"))
})

# testing df_diff must have two vectors of incremental probabilities

df_diff1 <- df_diff[, 1]

test_that("scatter_plot() expects an error when df_diff() is missing required
          vector(s)", {
  expect_error(scatter_plot(df_diff1, outcome))
})

# testing df_diff cannot take two identical vectors of incremental probabilities

df_diff3 <- data.frame(bdiff, bdiff)

test_that("scatter_plot() expects an error when df_diff() has two identical
          vectors of incremental probabilities", {
  expect_error(scatter_plot(df_diff3, outcome))
})

# testing scatter_plot's ability to handle missing data

df_diff2 <- df_diff
df_diff2[1, 1] <- NA

test_that("scatter_plot() will return a custom warning message concerning
missing data", {
  expect_warning(scatter_plot(df_diff2, outcome))
})

test_that("scatter_plot() will return a visible object with
missing data", {
  expect_visible(scatter_plot(df_diff2, outcome))
})
