#' Trade-off plot
#'
#' Generate trade-off plot
#' @param data (`data.frame`) input dataset
#' @param filter (`character`) selected filter
#' @param category (`character`) selected category
#' @param benefit (`character`) selected benefit outcome
#' @param risk (`character`) selected risk outcome
#' @param type_risk (`character`) selected way to display risk outcomes
#' (crude proportions, Exposure-adjusted rates (per 100 PYs))
#' @param type_graph (`character`) selected way to display binary outcomes
#' (Absolute risk, Relative risk, Odds ratio)
#' @param ci (`character`) selected choice to display confidence intervals
#' or not (Yes, No)
#' @param ci_method (`character`) selected method to display
#' confidence intervals (Supplied, Calculated)
#' @param mab (`numeric`) specified minimum acceptable benefit
#' @param mar (`numeric`) specified maximum acceptable risk
#' @param threshold (`character`) selected way to set benefit-risk threshold
#' (None, Straight line, Segmented line, Smooth curve)
#' @param ratio (`numeric`) specified maximum acceptable ratio
#' between risk and benefit
#' @param b1,b2,b3,b4,b5,b6,b7,b8,b9,b10 (`numeric`) specified benefit
#' @param r1,r2,r3,r4,r5,r6,r7,r8,r9,r10 (`numeric`)
#'   specified risk tolerance
#' @param testdrug (`character`) selected choice to display test drug or not
#' (Yes, No)
#' @param type_scale (`character`) selected scale display type (Fixed, Free)
#' @param lower_x,upper_x,lower_y,upper_y (`numeric`) specified axis limits
#' @param chartcolors (`vector`) a vector of colors, the same number of levels
#' as the number of treatments
#'
#' @return a ggplot object
#' @import shiny
#' @export

#' @examples
#' generate_tradeoff_plot(
#'   data = effects_table, filter = "None", category = "All",
#'   benefit = "Primary Efficacy", risk = "Reoccurring AE",
#'   type_risk = "Crude proportions", type_graph = "Absolute risk",
#'   ci = "Yes", ci_method = "Calculated",
#'   mab = 0.05,
#'   mar = 0.45,
#'   threshold = "Segmented line",
#'   ratio = 4,
#'   b1 = 0.05,
#'   b2 = 0.1,
#'   b3 = 0.15,
#'   b4 = 0.2,
#'   b5 = 0.25,
#'   b6 = 0.3,
#'   b7 = 0.35,
#'   b8 = 0.4,
#'   b9 = 0.45,
#'   b10 = 0.5,
#'   r1 = 0.09,
#'   r2 = 0.17,
#'   r3 = 0.24,
#'   r4 = 0.3,
#'   r5 = 0.35,
#'   r6 = 0.39,
#'   r7 = 0.42,
#'   r8 = 0.44,
#'   r9 = 0.45,
#'   r10 = 0.45,
#'   testdrug = "Yes",
#'   type_scale = "Free",
#'   lower_x = 0,
#'   upper_x = 0.5,
#'   lower_y = 0,
#'   upper_y = 0.5,
#'   chartcolors <- colfun()$fig7_colors
#' )
#'
generate_tradeoff_plot <- function(data, filter, category, benefit, risk,
                                   type_risk, type_graph, ci, ci_method, mab,
                                   mar, threshold, ratio, b1, b2, b3, b4, b5,
                                   b6, b7, b8, b9, b10, r1, r2, r3, r4, r5, r6,
                                   r7, r8, r9, r10, testdrug, type_scale,
                                   lower_x, upper_x, lower_y, upper_y,
                                   chartcolors) {
  # preparing data for the tradeoff plot
  df_br <- prepare_tradeoff_data(
    data, filter, category, benefit, risk,
    ci_method, type_risk, type_graph
  )

  # set the axis limits for the benefit/risk outcomes
  if (type_scale == "Fixed") {
    brx_min <- min(df_br$benefit_lowerCI, df_br$benefit, na.rm = TRUE)
    brx_max <- max(df_br$benefit_upperCI, df_br$benefit, na.rm = TRUE)

    x_min <- relmin(brx_min, type_scale = "Fixed")
    x_max <- relmax(brx_max, type_scale = "Fixed")

    # log10 scale doesn't work when x_min is 0
    if ((type_graph == "Relative risk" | type_graph == "Odds ratio") &
      unique(df_br$benefit_Type) == "Binary" & x_min == 0) {
      x_min <- 0.01
      x_max <- max(0.01, x_max)
    }

    bry_min <- min(df_br$risk_lowerCI, df_br$risk, na.rm = TRUE)
    bry_max <- max(df_br$risk_upperCI, df_br$risk, na.rm = TRUE)

    y_min <- relmin(bry_min, type_scale = "Fixed")
    y_max <- relmax(bry_max, type_scale = "Fixed")

    # log10 scale doesn't work when y_min is 0
    if ((type_graph == "Relative risk" | type_graph == "Odds ratio") &
      type_risk == "Crude proportions" & y_min == 0) {
      y_min <- 0.01
      y_max <- max(y_max, 0.01)
    }

    if (unique(df_br$benefit_Type) == "Binary" & type_graph == "Absolute risk") {
      if (brx_min >= 0 & bry_min >= 0) {
        x_min <- 0
        x_max <- 1
      } else if (brx_max <= 0 & bry_max <= 0) {
        x_min <- -1
        x_max <- 0
      } else {
        x_min <- -1
        x_max <- 1
      }
    }

    if (type_risk == "Crude proportions" & type_graph == "Absolute risk") {
      if (brx_min >= 0 & bry_min >= 0) {
        y_min <- 0
        y_max <- 1
      } else if (brx_max <= 0 & bry_max <= 0) {
        y_min <- -1
        y_max <- 0
      } else {
        y_min <- -1
        y_max <- 1
      }
    }
  } else if (type_scale == "Free") {
    error_msg <- paste0(
      ifelse(!is.na(lower_x), "",
        "please enter a numeric number for lower limit x axis; "
      ),
      ifelse(!is.na(upper_x), "",
        "please enter a numeric number for upper limit x axis; "
      ),
      ifelse(!is.na(lower_y), "",
        "please enter a numeric number for lower limit y axis; "
      ),
      ifelse(!is.na(upper_y), "",
        "please enter a numeric number for upper limit y axis; "
      ),
      ifelse((type_graph == "Relative risk" | type_graph == "Odds ratio") &
        unique(df_br$benefit_Type) == "Binary" & lower_x <= 0,
      "please enter a positive value for lower limit x axis; ",
      ""
      ),
      ifelse((type_graph == "Relative risk" | type_graph == "Odds ratio") &
        type_risk == "Crude proportions" & lower_y <= 0,
      "please enter a positive value for lower limit y axis; ",
      ""
      ),
      ifelse((!is.na(lower_x)) & (!is.na(upper_x)) & (lower_x < upper_x), "",
        "the lower limit x axis should be less than upper limit x axis; "
      ),
      ifelse((!is.na(lower_y)) & (!is.na(upper_y)) & (lower_y < upper_y), "",
        "the lower limit y axis should be less than upper limit y axis"
      )
    )

    validate(
      need(error_msg == "", error_msg)
    )

    x_min <- lower_x
    x_max <- upper_x
    y_min <- lower_y
    y_max <- upper_y
  }

  # create tradeoff plot
  myplot <- ggplot()

  # prepare the tradeoff plot
  # generate b/r points for approved drugs
  if (testdrug == "No") {
    if (sum(df_br$Drug_Status == "Approved") > 0) {
      myplot <- prepare_tradeoff_plot(
        myplot = myplot,
        data = data,
        df_br = df_br,
        drug_status = "Approved",
        filter = filter,
        ci = ci,
        chartcolors = chartcolors
      )
    }

    # generate b/r points for both approved drugs and tested drugs
  } else if (testdrug == "Yes") {
    myplot <- prepare_tradeoff_plot(
      myplot = myplot,
      data = data,
      df_br = df_br,
      drug_status = c("Approved", "Test"),
      filter = filter,
      ci = ci,
      chartcolors = chartcolors
    )
  }

  # quadrants
  if (!is.na(mab)) {
    myplot <- myplot +
      annotate("rect",
        xmin = x_min, xmax = mab, ymin = y_min,
        ymax = y_max, fill = "grey", alpha = 0.7
      )
  }
  if (!is.na(mar)) {
    myplot <- myplot +
      annotate("rect",
        xmin = x_min, xmax = x_max, ymin = mar,
        ymax = y_max, fill = "grey", alpha = 0.7
      )
  }

  # set up the benefit/risk tradeoff threshold
  if (threshold == "None") {
    myplot <- myplot
  } else if (threshold == "Straight line") {
    # display the threshold as a straight line
    if (x_min * ratio >= y_min) {
      x_line_min <- x_min
      y_line_min <- x_min * ratio
    } else {
      x_line_min <- c(x_min, y_min / ratio)
      y_line_min <- c(y_min, y_min)
    }

    if (x_max * ratio <= y_max) {
      x_line_max <- x_max
      y_line_max <- x_max * ratio
    } else {
      x_line_max <- y_max / ratio
      y_line_max <- y_max
    }

    x_line <- c(x_line_min, x_line_max)
    y_line <- c(y_line_min, y_line_max)
    df_line <- data.frame(x_line, y_line)

    myplot <- myplot +
      geom_line(aes(x = x_line, y = y_line),
        data = df_line,
        linetype = 1, size = 1.2, colour = "maroon4"
      ) +
      geom_ribbon(aes(x = x_line, ymin = y_line, ymax = y_max),
        data = df_line,
        fill = "grey", alpha = 0.7
      )
  } else {
    x_curve <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
    y_curve <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)

    my_warning <- "At least one of the benefit and risk threshold values you entered to set up segmented line or smooth curve is outside of the axis limits. Please make corrections by revising the values or resetting the axis limits using free scale."

    error_msg <- paste0(
      ifelse(min(x_curve) < x_min | max(x_curve) > x_max |
        min(y_curve) < y_min | max(y_curve) > y_max,
      my_warning, ""
      )
    )

    validate(
      need(error_msg == "", error_msg)
    )

    if (!x_min %in% x_curve) {
      x_curve <- c(x_min, x_curve)
      y_curve <- c(y_min, y_curve)
    }

    if (!x_max %in% x_curve) {
      x_curve <- c(x_curve, x_max)
      y_curve <- c(y_curve, min(mar, y_max, na.rm = TRUE))
    }

    df_curve <- data.frame(x_curve, y_curve)
    df_curve <- df_curve %>%
      filter(!is.na(x_curve) & !is.na(y_curve))

    if (threshold == "Segmented line") {
      # display the threshold as a segmented line
      myplot <- myplot +
        geom_line(aes(x = x_curve, y = y_curve),
          data = df_curve,
          linetype = 1, size = 1.2, colour = "maroon4"
        ) +
        geom_point(aes(x = x_curve, y = y_curve), df_curve,
          colour = "black", size = 2, shape = 15
        ) +
        geom_ribbon(aes(x = x_curve, ymin = y_curve, ymax = y_max),
          data = df_curve, fill = "grey", alpha = 0.7
        )
    } else if (threshold == "Smooth curve") {
      # display the threshold as a smooth curve
      new_df_curve <- expand.grid(x_curve = seq(x_min, x_max, by = 0.01))
      new_df_curve$y_curve <- stats::predict(stats::loess(y_curve ~ x_curve),
        data = df_curve,
        newdata = new_df_curve
      )
      myplot <- myplot +
        geom_smooth(aes(x = x_curve, y = y_curve),
          data = df_curve, se = FALSE,
          linetype = 1, size = 1.2, colour = "maroon4"
        ) +
        geom_point(aes(x = x_curve, y = y_curve),
          data = df_curve,
          colour = "black", size = 2, shape = 15
        ) +
        geom_ribbon(
          aes(
            x = x_curve,
            ymin = y_curve,
            ymax = y_max
          ),
          data = new_df_curve, fill = "grey",
          alpha = 0.7
        )
    }
  }

  # create segments for both minimal acceptable risk and minimal acceptable
  if (!is.na(mab)) {
    myplot <- myplot +
      geom_segment(aes(x = mab, xend = mab, y = y_min, yend = y_max),
        linetype = 2, colour = "darkorange3", size = 1.2
      )
  }
  if (!is.na(mar)) {
    myplot <- myplot +
      geom_segment(aes(x = x_min, xend = x_max, y = mar, yend = mar),
        linetype = 2, colour = "darkorange3", size = 1.2
      )
  }

  # create segments for graph margins
  myplot <- myplot +
    geom_segment(aes(x = x_min, xend = x_max, y = y_max, yend = y_max),
      linetype = 2, colour = "darkorange3", size = 1.2
    ) +
    geom_segment(aes(x = x_max, xend = x_max, y = y_min, yend = y_max),
      linetype = 2, colour = "darkorange3", size = 1.2
    ) +
    geom_segment(aes(x = x_min, xend = x_min, y = y_min, yend = y_max),
      linetype = 1, size = 1
    ) +
    geom_segment(aes(x = x_min, xend = x_max, y = y_min, yend = y_min),
      linetype = 1, size = 1
    )

  # labels
  if (!is.na(mar)) {
    myplot <- myplot +
      geom_text(aes(label = "MAR", x = x_max, y = mar), size = control_fonts()$p * 0.35, hjust = -0.15)
  }
  if (!is.na(mab)) {
    myplot <- myplot +
      geom_text(aes(label = "MAB", x = mab, y = y_max), size = control_fonts()$p * 0.35, vjust = -0.3)
  }
  myplot <- myplot + xlab(benefit) + ylab(risk)

  # x coordinates - the 0.03 part is to leave room to display the "MAR" label
  if ((type_graph == "Relative risk" | type_graph == "Odds ratio") &
    unique(df_br$benefit_Type) == "Binary") {
    myplot <- myplot +
      scale_x_log10(
        limits = c(
          x_min, x_max * (10^(0.03 * (log10(x_max) - log10(x_min))))
        ),
        labels = scales::label_number(accuracy = 0.01)
      )
  } else {
    myplot <- myplot +
      scale_x_continuous(limits = c(x_min, x_max + 0.03 * (x_max - x_min)))
  }

  # y coordinates
  if ((type_graph == "Relative risk" | type_graph == "Odds ratio") &
    type_risk == "Crude proportions") {
    myplot <- myplot +
      scale_y_log10(
        limits = c(y_min, y_max),
        labels = scales::label_number(accuracy = 0.01)
      )
  } else {
    myplot <- myplot +
      scale_y_continuous(limits = c(y_min, y_max))
  }

  # update the plot
  message(glue('[{format(Sys.time(),"%F %T")}] > update final trade-off plot'))

  myplot +
    # theme_minimal() +
    # theme(
    #   axis.text = element_text(size = 14),
    #   axis.title = element_text(size = 16, face = "bold"),
    #   plot.margin = unit(c(1, 2, 2, 1), "cm")
    # ) +
    br_charts_theme(
      axis_line = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    coord_cartesian(clip = "off") +
    guides(
      color = guide_legend(title = "Treatment:"),
      shape = guide_legend(title = "Treatment:")
    )
}

#' Prepare trade-off plot
#'
#' Add points and dual CIs to trade-off plot
#' @param myplot raw plot
#' @param data (`data.frame`) input dataset
#' @param df_br (`data.frame`) processed dataset
#' @param drug_status (`character`) selected status of drug to display
#' (Approved, Test)
#' @param filter (`character`) selected filter
#' @param ci (`character`) selected choice to display confidence intervals
#' or not (Yes, No)
#' @param chartcolors (`vector`) a vector of colors, the same number of levels
#' as the number of treatments
#'
#' @rdname prepare_tradeoff_plot
#' @export
prepare_tradeoff_plot <- function(myplot, data, df_br, drug_status, filter, ci,
                                  chartcolors) {
  # get all the treatments that comply with the status to display
  if (filter != "None") {
    df_br_status <- df_br %>%
      filter(Drug_Status %in% drug_status) %>%
      mutate(treatment = paste0(treatment, " : ", category))
  } else {
    df_br_status <- df_br %>%
      filter(Drug_Status %in% drug_status)
  }

  df_br_status$treatment <- factor(df_br_status$treatment,
    levels = c(unique(df_br_status$treatment))
  )

  # set a color for each treatment
  if (filter != "None") {
    data <- data %>%
      mutate(treatment = paste0(Trt1, " : ", Category))
  } else {
    data <- data %>%
      mutate(treatment = Trt1)
  }

  if (filter != "None") {
    my_colors <- rep(chartcolors, each = length(levels(as.factor(data$Category))))
    names(my_colors) <- c(t(outer(levels(as.factor(data$Trt1)),
      levels(as.factor(data$Category)), paste,
      sep = " : "
    )))
  } else {
    my_colors <- chartcolors
    names(my_colors) <- c(levels(as.factor(data$Trt1)))
  }

  # map the color with the different treatments
  col_scale <- scale_color_manual(
    name = "treatment",
    values = as.vector(my_colors[as.character(df_br_status$treatment)]),
    guide = guide_legend(title = "Treatment")
  )

  # set a shape for each category
  my_shapes <- c(16, 17, 15, 18, 3, 4, 8, 11)[1:length(levels(as.factor(data$Category)))]
  names(my_shapes) <- c(levels(as.factor(data$Category)))

  # map the shape with the different category
  shape_scale <- scale_shape_manual(
    name = "treatment",
    values = as.vector(my_shapes[as.character(df_br_status$category)]),
    guide = guide_legend(title = "Treatment")
  )

  message(glue('[{format(Sys.time(),"%F %T")}] > prepare trade-off plot'))

  if (ci == "No") {
    # create a scatterplot displaying the benefit/risk metrics without
    # plotting their respective confidence intervals
    myplot +
      geom_point(
        aes_string(
          x = "benefit", y = "risk", colour = "treatment",
          shape = "treatment"
        ),
        data = df_br_status,
        size = 3,
        show.legend = TRUE
      ) +
      col_scale +
      shape_scale
  } else {
    # create a scatterplot displaying the benefit/risk metrics with
    # their respective confidence intervals
    # create a scatterplot given the risk/benefit metrics
    myplot +
      geom_point(
        aes_string(
          x = "benefit", y = "risk", colour = "treatment", shape = "treatment"
        ),
        data = df_br_status,
        size = 3,
        show.legend = TRUE
      ) +

      # add a segment to each point representing the confidence intervals for
      # metrics
      geom_segment(
        aes_string(
          x = "benefit_lowerCI", xend = "benefit",
          y = "risk", yend = "risk", colour = "treatment"
        ),
        data = df_br_status,
        size = 1,
        show.legend = FALSE
      ) +
      geom_segment(
        aes_string(
          x = "benefit_upperCI", xend = "benefit",
          y = "risk", yend = "risk", colour = "treatment"
        ),
        data = df_br_status,
        size = 1,
        show.legend = FALSE
      ) +
      geom_segment(
        aes_string(
          x = "benefit", xend = "benefit",
          y = "risk_lowerCI", yend = "risk", colour = "treatment"
        ),
        data = df_br_status,
        size = 1,
        show.legend = FALSE
      ) +
      geom_segment(
        aes_string(
          x = "benefit", xend = "benefit",
          y = "risk_upperCI", yend = "risk", colour = "treatment"
        ),
        data = df_br_status,
        size = 1,
        show.legend = FALSE
      ) +
      col_scale +

      # make each point filled with white points
      # geom_point(
      #   aes_string(x = "benefit", y = "risk", shape = "treatment"),
      #   data = df_br_status,
      #   colour = "white",
      #   size = 2,
      #   show.legend = TRUE
      # ) +
      shape_scale
  }
}

#' Prepare data for the tradeoff plot
#'
#' @param data (`data.frame`) dataset
#' @param filter (`character`) selected filter
#' @param category (`character`) selected category
#' @param benefit (`character`) selected benefit outcome
#' @param risk (`character`) selected risk outcome
#' @param ci_method (`character`) selected method to display
#' confidence intervals
#' @param ci_method (`character`) selected method to display
#' confidence intervals
#' @param type_risk (`character`) selected way to display risk outcomes
#' (crude proportions, Exposure-adjusted rates (per 100 PYs))
#' @param type_graph (`character`) selected way to display binary outcomes
#' (Absolute risk, Relative risk, Odds ratio)
#' @return df_br (`data.frame`) benefit/risk metrics for all treatment given
#' the selected benefit (respectively risk) outcome
#' @details DETAILS
#' @rdname prepare_tradeoff_data
#' @import shiny
#' @export
prepare_tradeoff_data <- function(data, filter, category, benefit, risk,
                                  ci_method, type_risk, type_graph) {
  # control the data quality before plotting the tradeoff plot

  error_msg <- paste0(
    check_feature_string(
      data = data, feature = "Outcome", plots = "tradeoff",
      func = is.character, na_check = T,
      check_unique = c(
        "Factor", "Grouped_Outcome",
        "Statistics", "Outcome_Status",
        "Type"
      )
    ),
    check_feature_string(
      data = data, feature = "Factor",
      plots = "tradeoff",
      func = is.character, na_check = T,
      values = c("Benefit", "Risk")
    ),
    check_feature_string(
      data = data, feature = "Filter", plots = "tradeoff",
      func = is.character, na_check = T
    ),
    check_feature_string(
      data = data, feature = "Drug_Status",
      plots = "tradeoff",
      func = is.character, values = c("Approved", "Test")
    ),
    check_feature_string(
      data = data, feature = "Category",
      plots = "tradeoff",
      func = is.character, na_check = T
    ),
    check_feature_string(
      data = data, feature = "Trt1",
      plots = "tradeoff", func = is.character,
      na_check = T
    ),
    check_feature_string(
      data = data, feature = "Trt2",
      plots = "tradeoff",
      func = is.character, na_check = T,
      check_same = T
    ),
    check_feature_string(
      data = data, feature = "Type", plots = "tradeoff",
      func = is.character, na_check = T,
      values = c("Binary", "Continuous")
    )
  )

  validate(need(error_msg == "", error_msg))


  # subset the data based on a selected filter/category if applicable

  if (filter == "None") {
    df_filter <- data %>%
      filter(Filter == "None")
  } else {
    df_filter <- data %>%
      filter(Filter == filter & Category %in% category)
  }

  validate(
    need(
      nrow(df_filter) > 0,
      "filtered effects table is empty"
    )
  )


  # subset data based on the selected benefit outcome
  df_benefit <- df_filter[df_filter$Outcome == benefit, ]

  validate(
    need(
      nrow(df_benefit) > 0,
      "benefit table is empty : there is no benefit outcome in the dataset"
    )
  )

  # subset data based on the selected risk outcome
  df_risk <- df_filter[df_filter$Outcome == risk, ]

  validate(
    need(
      nrow(df_risk) > 0,
      "risk table is empty : there is not risk outcome in the dataset"
    )
  )

  # extract result table for both benefit/risk outcomes :
  # Process data for benefit and fetch provisioned interval confidence

  if (ci_method == "Supplied") {
    if (df_benefit$Type[1] == "Continuous") {
      error_msg <- paste0(
        check_feature_string(
          data = df_benefit, feature = "Mean1",
          plots = "tradeoff",
          func = is.numeric
        ),
        check_feature_string(
          data = df_benefit, feature = "Mean2",
          plots = "tradeoff",
          func = is.numeric
        ),
        check_feature_string(
          data = df_benefit, feature = "Diff_LowerCI",
          plots = "tradeoff", func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        ),
        check_feature_string(
          data = df_benefit, feature = "Diff_UpperCI",
          plots = "tradeoff", func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        )
      )

      validate(need(error_msg == "", error_msg))

      # derive mean difference and get the associated confidence intervals
      # for continuous benefit outcome
      df_benefit <- prepare_br_supplied_ci(
        df_benefit, "Mean", "Diff", function(x, y) x - y
      )
    } else if (df_benefit$Type[1] == "Binary") {
      # derive probability metrics (Absolute Risk, Relative Risk and Odds ratio
      # for binary benefit outcome)

      error_msg <- paste0(
        check_feature_string(
          data = df_benefit, feature = "Prop1",
          plots = "tradeoff", func = is.numeric,
          check_range = c(0, 1)
        ),
        check_feature_string(
          data = df_benefit, feature = "Prop2",
          plots = "tradeoff", func = is.numeric,
          check_range = c(0, 1)
        )
      )

      validate(need(error_msg == "", error_msg))

      if (type_graph == "Absolute risk") {
        error_msg <- paste0(
          check_feature_string(
            data = df_benefit, feature = "Diff_LowerCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_benefit, feature = "Diff_UpperCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )

        validate(need(error_msg == "", error_msg))

        df_benefit <- prepare_br_supplied_ci(
          df_benefit, "Prop", "Diff", function(x, y) x - y
        )
      } else if (type_graph == "Relative risk") {
        error_msg <- paste0(
          check_feature_string(
            data = df_benefit, feature = "RelRisk_LowerCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_benefit, feature = "RelRisk_UpperCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )

        validate(need(error_msg == "", error_msg))

        df_benefit <- prepare_br_supplied_ci(
          df_benefit, "Prop", "RelRisk", function(x, y) x / y
        )
      } else if (type_graph == "Odds ratio") {
        error_msg <- paste0(
          check_feature_string(
            data = df_benefit, feature = "OddsRatio_LowerCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_benefit, feature = "OddsRatio_UpperCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )

        validate(need(error_msg == "", error_msg))

        df_benefit <- prepare_br_supplied_ci(
          df_benefit, "Prop", "OddsRatio", function(x, y) {
            (x * (1 - y)) / (y * (1 - x))
          }
        )
      }
    }

    # Process data for risk and fetch provisioned interval confidence
    if (type_risk == "Crude proportions") {
      if (type_graph == "Absolute risk") {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk, feature = "Diff_LowerCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_risk, feature = "Diff_UpperCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )

        validate(need(error_msg == "", error_msg))


        df_risk <- prepare_br_supplied_ci(
          df_risk, "Prop", "Diff", function(x, y) x - y
        )
      } else if (type_graph == "Relative risk") {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk, feature = "RelRisk_LowerCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_risk, feature = "RelRisk_UpperCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )

        validate(need(error_msg == "", error_msg))

        df_risk <- prepare_br_supplied_ci(
          df_risk, "Prop", "RelRisk", function(x, y) x / y
        )
      } else if (type_graph == "Odds ratio") {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk, feature = "OddsRatio_LowerCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_risk, feature = "OddsRatio_UpperCI",
            plots = "tradeoff", func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )

        validate(need(error_msg == "", error_msg))

        df_risk <- prepare_br_supplied_ci(
          df_risk, "Prop", "OddsRatio", function(x, y) {
            (x * (1 - y)) / (y * (1 - x))
          }
        )
      }
    } else if (type_risk == "Exposure-adjusted rates (per 100 PYs)") {
      df_risk <- df_risk[!is.na(df_risk$Rate_Type), ]

      error_msg <- paste0(
        check_feature_string(
          data = df_risk,
          feature = "Rate_Type",
          plots = "tradeoff",
          func = is.character,
          values = c("EventRate", "IncRate")
        )
      )

      validate(need(error_msg == "", error_msg))

      if (nrow(df_risk[df_risk$Rate_Type == "EventRate", ]) > 0) {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "EventRate1",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "EventRate2",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "Diff_EventRate_LowerCI",
            plots = "tradeoff",
            func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "Diff_EventRate_UpperCI",
            plots = "tradeoff",
            func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )
        validate(need(error_msg == "", error_msg))
      }

      if (nrow(df_risk[df_risk$Rate_Type == "IncRate", ]) > 0) {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "IncRate1",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "IncRate2",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "Diff_IncRate_LowerCI",
            plots = "tradeoff",
            func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "Diff_IncRate_UpperCI",
            plots = "tradeoff",
            func = is.numeric,
            add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
          )
        )
        validate(need(error_msg == "", error_msg))
      }

      df_event_rate_risk <- df_risk[df_risk$Rate_Type == "EventRate", ]
      df_event_rate_risk <- prepare_br_supplied_ci(
        df_event_rate_risk, "EventRate", "Diff_EventRate", function(x, y) x - y
      )

      df_inc_rate_risk <- df_risk[df_risk$Rate_Type == "IncRate", ]
      df_inc_rate_risk <- prepare_br_supplied_ci(
        df_inc_rate_risk, "IncRate", "Diff_IncRate", function(x, y) x - y
      )

      df_risk <- rbind(df_event_rate_risk, df_inc_rate_risk)
    }
  } else if (ci_method == "Calculated") {
    # Process data for benefit with calculated interval confidence
    if (df_benefit$Type[1] == "Continuous") {
      # derive mean difference and calculate the associated confidence intervals
      # for continuous benefit outcome

      error_msg <- paste0(
        check_feature_string(
          data = df_benefit, feature = "Mean1",
          plots = "tradeoff",
          func = is.numeric
        ),
        check_feature_string(
          data = df_benefit, feature = "Mean2",
          plots = "tradeoff",
          func = is.numeric
        ),
        check_feature_string(
          data = df_benefit, feature = "N1",
          plots = "tradeoff",
          func = is.integer, check_positive = T
        ),
        check_feature_string(
          data = df_benefit, feature = "N2",
          plots = "tradeoff",
          func = is.integer, check_positive = T
        ),
        check_feature_string(
          data = df_benefit, feature = "Sd1",
          plots = "tradeoff",
          func = is.numeric, check_positive = T
        ),
        check_feature_string(
          data = df_benefit, feature = "Sd2",
          plots = "tradeoff",
          func = is.numeric, check_positive = T
        )
      )

      validate(need(error_msg == "", error_msg))

      df_benefit <- data.frame(
        benefit_Type = df_benefit$Type,
        Category = df_benefit$Category,
        Trt1 = df_benefit$Trt1,
        calculate_diff_con(
          mean1 = df_benefit$Mean1,
          mean2 = df_benefit$Mean2,
          sd1 = df_benefit$Sd1,
          sd2 = df_benefit$Sd2,
          N1 = df_benefit$N1,
          N2 = df_benefit$N2
        )
      )

      colnames(df_benefit)[which(colnames(df_benefit) == "diff")] <- "benefit"
      colnames(df_benefit)[which(colnames(df_benefit) == "lower")] <-
        "benefit_lowerCI"
      colnames(df_benefit)[which(colnames(df_benefit) == "upper")] <-
        "benefit_upperCI"
    } else if (df_benefit$Type[1] == "Binary") {
      # derive probability metrics (Absolute Risk, Relative Risk and Odds ratio
      # for binary benefit outcome) and calculate the associate confidence
      # intervals


      error_msg <- paste0(
        check_feature_string(
          data = df_benefit, feature = "Prop1",
          plots = "tradeoff", func = is.numeric,
          check_range = c(0, 1)
        ),
        check_feature_string(
          data = df_benefit, feature = "Prop2",
          plots = "tradeoff", func = is.numeric,
          check_range = c(0, 1)
        ),
        check_feature_string(
          data = df_benefit, feature = "N1",
          plots = "tradeoff",
          func = is.integer, check_positive = T
        ),
        check_feature_string(
          data = df_benefit, feature = "N2",
          plots = "tradeoff",
          func = is.integer, check_positive = T
        )
      )

      validate(need(error_msg == "", error_msg))

      if (type_graph == "Absolute risk") {
        df_benefit <- prepare_br_calculated_ci(
          df_benefit, "Prop", "N", calculate_diff_bin
        )
      } else if (type_graph == "Relative risk") {
        df_benefit <- prepare_br_calculated_ci(
          df_benefit, "Prop", "N", calculate_rel_risk_bin
        )
      } else if (type_graph == "Odds ratio") {
        df_benefit <- prepare_br_calculated_ci(
          df_benefit, "Prop", "N", calculate_odds_ratio_bin
        )
      }
    }
    # Process data for risk with calculated interval confidence
    if (type_risk == "Crude proportions") {
      error_msg <- paste0(
        check_feature_string(
          data = df_risk, feature = "Prop1",
          plots = "tradeoff", func = is.numeric,
          check_range = c(0, 1)
        ),
        check_feature_string(
          data = df_risk, feature = "Prop2",
          plots = "tradeoff", func = is.numeric,
          check_range = c(0, 1)
        ),
        check_feature_string(
          data = df_risk, feature = "N1",
          plots = "tradeoff",
          func = is.integer, check_positive = T
        ),
        check_feature_string(
          data = df_risk, feature = "N2",
          plots = "tradeoff",
          func = is.integer, check_positive = T
        )
      )

      validate(need(error_msg == "", error_msg))

      if (type_graph == "Absolute risk") {
        df_risk <- prepare_br_calculated_ci(
          df_risk, "Prop", "N", calculate_diff_bin
        )
      } else if (type_graph == "Relative risk") {
        df_risk <- prepare_br_calculated_ci(
          df_risk, "Prop", "N", calculate_rel_risk_bin
        )
      } else if (type_graph == "Odds ratio") {
        df_risk <- prepare_br_calculated_ci(
          df_risk, "Prop", "N", calculate_odds_ratio_bin
        )
      }
    } else if (type_risk == "Exposure-adjusted rates (per 100 PYs)") {
      df_risk <- df_risk[!is.na(df_risk$Rate_Type), ]

      error_msg <- paste0(
        check_feature_string(
          data = df_risk,
          feature = "Rate_Type",
          plots = "tradeoff",
          func = is.character,
          values = c("EventRate", "IncRate")
        )
      )

      validate(need(error_msg == "", error_msg))

      if (nrow(df_risk[df_risk$Rate_Type == "EventRate", ]) > 0) {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "EventRate1",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "EventRate2",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "100PEY1",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "EventRate", ],
            feature = "100PEY2",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          )
        )
        validate(need(error_msg == "", error_msg))
      }

      if (nrow(df_risk[df_risk$Rate_Type == "IncRate", ]) > 0) {
        error_msg <- paste0(
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "IncRate1",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "IncRate2",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "100PYAR1",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          ),
          check_feature_string(
            data = df_risk[df_risk$Rate_Type == "IncRate", ],
            feature = "100PYAR2",
            plots = "tradeoff",
            func = is.numeric,
            na_check = T,
            check_positive = T
          )
        )
        validate(need(error_msg == "", error_msg))
      }

      df_event_rate_risk <- df_risk[df_risk$Rate_Type == "EventRate", ]
      df_event_rate_risk <- prepare_br_calculated_ci(
        df_event_rate_risk, "EventRate", "100PEY", calculate_diff_rates
      )

      df_inc_rate_risk <- df_risk[df_risk$Rate_Type == "IncRate", ]
      df_inc_rate_risk <- prepare_br_calculated_ci(
        df_inc_rate_risk, "IncRate", "100PYAR", calculate_diff_rates
      )

      df_risk <- rbind(df_event_rate_risk, df_inc_rate_risk)
    }
  }

  # subset the filtered data based on the selected benefit outcome and keep only
  # the treatment and drug status column

  df_drug <- df_filter %>%
    filter(Outcome == benefit) %>%
    select("Category", "Trt1", "Trt2", "Drug_Status")

  df_br <- merge(df_benefit, df_risk, by = c("Category", "Trt1"), sort = F)
  df_br <- merge(df_br, df_drug, by = c("Category", "Trt1"), sort = F)

  colnames(df_br)[which(colnames(df_br) == "Trt1")] <- "treatment"
  colnames(df_br)[which(colnames(df_br) == "Trt2")] <- "placebo"
  colnames(df_br)[which(colnames(df_br) == "Category")] <- "category"

  # bind all the selected treatment with the benefit/risk data points
  # df_br <- data.frame(df_benefit, df_risk, df_drug)

  # return the benefit/risk table and the intermediary tables processed
  message(glue('[{format(Sys.time(),"%F %T")}] > prepare tradeoff data'))
  df_br
}
