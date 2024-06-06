library(testthat)
mycolors <- c(colfun()$fig7_colors, "Ffff00")

test_that("test that tradeoff_plot returns a ggplot object when benefit-risk trade-off
          threshold is a straight line", {
  result <- generate_tradeoff_plot(
    data =brdata, filter = "None", category = "All",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Supplied", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Straight line",
    ratio = 4,
    testdrug = "Yes",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  )
  expect_true(inherits(result, "ggplot"))
})

test_that("test that tradeoff_plot returns a ggplot object when benefit-risk trade-off
          threshold is a smooth curve", {
  result <- generate_tradeoff_plot(
    data = brdata, filter = "None", category = "All",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Supplied", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Smooth curve",
    ratio = 4,
    b1 = 0.05,
    b2 = 0.1,
    b3 = 0.15,
    b4 = 0.2,
    b5 = 0.25,
    b6 = 0.3,
    b7 = 0.35,
    b8 = 0.4,
    b9 = 0.45,
    b10 = 0.5,
    r1 = 0.09,
    r2 = 0.17,
    r3 = 0.24,
    r4 = 0.3,
    r5 = 0.35,
    r6 = 0.39,
    r7 = 0.42,
    r8 = 0.44,
    r9 = 0.45,
    r10 = 0.45,
    testdrug = "Yes",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  )
  expect_true(inherits(result, "ggplot"))
})


test_that("test that tradeoff_plot returns a ggplot object when CI is calculated", {
  result <- generate_tradeoff_plot(
    data = brdata, filter = "None", category = "All",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Calculated", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Smooth curve",
    ratio = 4,
    b1 = 0.05,
    b2 = 0.1,
    b3 = 0.15,
    b4 = 0.2,
    b5 = 0.25,
    b6 = 0.3,
    b7 = 0.35,
    b8 = 0.4,
    b9 = 0.45,
    b10 = 0.5,
    r1 = 0.09,
    r2 = 0.17,
    r3 = 0.24,
    r4 = 0.3,
    r5 = 0.35,
    r6 = 0.39,
    r7 = 0.42,
    r8 = 0.44,
    r9 = 0.45,
    r10 = 0.45,
    testdrug = "Yes",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  )
  expect_true(inherits(result, "ggplot"))
})


test_that("test that tradeoff_plot returns a ggplot object for subgroup data", {
  result <- generate_tradeoff_plot(
    data = brdata, filter = "Gender", category = "Male",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Calculated", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Smooth curve",
    ratio = 4,
    b1 = 0.05,
    b2 = 0.1,
    b3 = 0.15,
    b4 = 0.2,
    b5 = 0.25,
    b6 = 0.3,
    b7 = 0.35,
    b8 = 0.4,
    b9 = 0.45,
    b10 = 0.5,
    r1 = 0.09,
    r2 = 0.17,
    r3 = 0.24,
    r4 = 0.3,
    r5 = 0.35,
    r6 = 0.39,
    r7 = 0.42,
    r8 = 0.44,
    r9 = 0.45,
    r10 = 0.45,
    testdrug = "No",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  )
  expect_true(inherits(result, "ggplot"))
})


brdata2 <- brdata
brdata2$Factor[brdata2$Outcome == "A (Primary Clinical Assessment)"] <- "NA"

test_that("test that tradeoff_plot throws an error when a required field is missing", {
  expect_error(generate_tradeoff_plot(
    data = brdata2, filter = "None", category = "All",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Calculated", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Straight line",
    ratio = 4,
    testdrug = "Yes",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  ),
  paste0(" Feature Factor must have the following values **Benefit, Risk** :",
  " errors occur in tradeoff plot(s);"), fixed = TRUE)
})

brdata3 <- brdata
brdata3$Prop1[brdata3$Filter == "None" &
                brdata3$Outcome == "A (Primary Clinical Assessment)" &
                brdata3$Trt1 == "Drug 1"] <- "0.45"

test_that("test that tradeoff_plot throws an error when data is not of required type", {
  expect_error(generate_tradeoff_plot(
    data = brdata3, filter = "None", category = "All",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Calculated", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Straight line",
    ratio = 4,
    testdrug = "Yes",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  ),
  "Feature Prop1 must be of type numeric : errors occur in tradeoff plot(s);",
  fixed = TRUE)
})

brdata4 <- brdata
brdata4$Prop1[brdata4$Filter == "None" &
                brdata4$Outcome == "A (Primary Clinical Assessment)" &
                brdata4$Trt1 == "Drug 1"] <- 1.45

test_that("test that tradeoff_plot throws an error when data is out of required range", {
  expect_error(generate_tradeoff_plot(
    data = brdata4, filter = "None", category = "All",
    benefit = "A (Primary Clinical Assessment)", risk = "J (A Common AE)",
    type_risk = "Crude proportions", type_graph = "Absolute risk",
    ci = "Yes", ci_method = "Calculated", cl = 0.95,
    mab = 0.05,
    mar = 0.45,
    threshold = "Straight line",
    ratio = 4,
    testdrug = "Yes",
    type_scale = "Free",
    lower_x = 0,
    upper_x = 0.5,
    lower_y = 0,
    upper_y = 0.5,
    chartcolors = mycolors
  ),
  " Feature Prop1 must be between [0, 1] : errors occur in tradeoff plot(s);",
  fixed = TRUE)
})
