library(testthat)

# testing CI for absolute risk for binary outcomes
test_that("calculate_diff_bin function test:
          Test for deriving mean difference and associated
          confidence intervals for binary outcomes", {
  expect_equal(
    calculate_diff_bin(0.45, 0.25, 500, 500, 0.95)
    [c("diff", "lower", "upper")],
    data.frame(
      diff = 0.2,
      lower = 0.1422, upper = 0.2578
    ),
    tolerance = 0.01
  )
})


# testing CI for log relative risk for binary outcomes
test_that("calculate_log_rel_risk_bin -
          Test for deriving log relative risk and
          associated confidence intervals ", {
  expect_equal(
    calculate_log_rel_risk_bin(0.45, 0.25, 500, 500, 0.95)
    [c("diff", "lower", "upper")],
    data.frame(
      diff = 0.5878,
      lower = 0.4077, upper = 0.7679
    ),
    tolerance = 0.01
  )
})


# testing CI for relative risk for binary outcomes
test_that("calculate_rel_risk_bin -
          Test for deriving relative risk and
          associated confidence intervals ", {
  expect_equal(
    calculate_rel_risk_bin(0.45, 0.25, 500, 500, 0.95)
    [c("rr", "lower", "upper")],
    data.frame(
      rr = 1.8,
      lower = 1.5033, upper = 2.1552
    ),
    tolerance = 0.01
  )
})


# testing CI for log odds ratio for binary outcomes
test_that("calculate_log_odds_ratio_bin -
          Test for deriving log odds ratio and
          associated confidence intervals ", {
  expect_equal(
    calculate_log_odds_ratio_bin(0.45, 0.25, 500, 500, 0.95)
    [c("diff", "lower", "upper")],
    data.frame(
      diff = 0.8979,
      lower = 0.6296, upper = 1.1663
    ),
    tolerance = 0.01
  )
})


# testing CI for odds ratio for binary outcomes
test_that("calculate_odds_ratio_bin -
          Test for deriving odds ratio and
          associated confidence intervals ", {
  expect_equal(
    calculate_odds_ratio_bin(0.45, 0.25, 500, 500, 0.95)
    [c("or", "lower", "upper")],
    data.frame(
      or = 2.4545,
      lower = 1.8768, upper = 3.2101
    ),
    tolerance = 0.01
  )
})


# testing CI for treatment difference in continuous outcomes
test_that("calculate_diff_con function test:
          Test for deriving mean difference and associated
          confidence intervals for continuous outcomes", {
  expect_equal(
    calculate_diff_con(
      mean1 = 0.6, mean2 = 0.5,
      sd1 = 0.1, sd2 = 0.3,
      N1 = 400, N2 = 500, cl = 0.95
    )
    [c("diff", "lower", "upper")],
    data.frame(
      diff = 0.1,
      lower = 0.0693, upper = 0.1307
    ),
    tolerance = 0.01
  )
})


# testing CI for treatment difference in exposure-adjusted rates
test_that("calculate_diff_rates function test:
          Test for deriving mean difference and associated
          confidence intervals for exposure-adjusted rates", {
  expect_equal(
    calculate_diff_rates(
      rate1 = 152.17, rate2 = 65.21,
      py1 = 230, py2 = 230,
      cl = 0.95
    )
    [c("diff", "lower", "upper")],
    data.frame(
      diff = 86.96,
      lower = 85.0545, upper = 88.8655
    ),
    tolerance = 0.01
  )
})
