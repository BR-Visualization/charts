#' Create a cumulative excess plot from a given dataframe
#'
#' @param df_outcome A dataframe with 6 variables named the following:
#' 1) eventtime: A vector of time points at which an event occurred.
#' 2) diff: A vector containing the difference in active and control effects.
#' 3) obsv_duration: A variable that specifies the duration of the
#' observational period (numerical).
#' 4) obsv_unit: A variable that specifies the unit for the duration of the
#' observational period (this is a non-numerical input).
#' 5) outcome: A vector containing whether the outcome is a "Benefit" or "Risk".
#' 6) eff_diff_lbl: A vector containing the label for effect difference.
#' @param subjects A numerical input that specifies the baseline proportion
#' of subjects in the study (for example, "per 100 subjects")
#' @param visits A numerical input that is the length between normal visits.
#' @param fig_colors Allows the user to change the colors of the figure
#' (defaults are provided). Must be vector of length 2, with color corresponding
#' to benefit first and risk second.
#' @param titlename Allows the user to change the documentation of the title
#' (default is provided).
#' @param ben_name Allows user to specify benefit of interest
#' (default is provided).
#' @param risk_name Allows user to specify risk of interest
#' (default is provided).
#' @param legend_position Allows user to specify legend position. Must be a
#' vector of length 2, with the first value corresponding to the position of the
#' legend relative to the x-axis, and the second corresponding to the position
#' of the legend relative to the y-axis (numeric).
#'
#' @return A cumulative excess plot.
#' @export
#' @import cowplot
#' @import ggplot2
#' @import simsurv
#' @import dplyr
#' @import magrittr
#' @importFrom stats sd
#' @importFrom ggrepel geom_label_repel
#' @importFrom forcats fct_reorder
#' @import reshape2
#' @import tidyverse
#' @import colorBlindness
#'
#' @examples
#' gensurv_plot(cumexcess, 100, 6,
#'   titlename =
#'     "Cumulative Excess # of Subjects w/ Events (per 100 Subjects)"
#' )
gensurv_plot <- function(
    df_outcome,
    subjects,
    visits,
    fig_colors = c("#0571b0", "#ca0020"),
    titlename = NULL, ben_name = "Primary Efficacy",
    risk_name = "Recurring AE",
    legend_position = c(-0.03, 1.15)) {
  outcome <- active <- comparator <- NULL
  eventtime <- obsv_duration <- obsv_unit <- eff_diff_lbl <- NULL

  all_columns <- c(
    "obsv_duration", "eventtime", "diff", "obsv_unit",
    "outcome", "eff_diff_lbl"
  )
  nonexistent_columns <- setdiff(all_columns, colnames(df_outcome))

  if (length(nonexistent_columns) > 0) {
    error_message <- paste0("You are missing a required variable in your
                            dataframe:", nonexistent_columns)
    stop(error_message)
  }

  df_outcome %>% select(
    eventtime, diff, obsv_duration, obsv_unit, outcome,
    eff_diff_lbl
  )

  if (any(is.na(df_outcome))) {
    warning(paste(
      "you have a missing value in row(s)",
      which(rowSums(is.na(df_outcome)) > 0)
    ))
    df_outcome <- na.omit(df_outcome)
  }

  active <- strsplit(unique(df_outcome$eff_diff_lbl), "-")[[1]][1]
  comparator <- strsplit(unique(df_outcome$eff_diff_lbl), "-")[[1]][2]

  df_ben <- df_outcome %>% dplyr::filter(outcome == "Benefit")
  mean1 <- mean(df_ben$diff)
  std1 <- sd(df_ben$diff)
  bmin <- mean1 - (3 * std1)
  bmax <- mean1 + (3 * std1)
  error_rows <- which(!(df_ben$diff > bmin) | !(df_ben$diff < bmax))
  if (!all(df_ben$diff > bmin) && !all(df_ben$diff < bmax)) {
    stop(paste("Custom error message: there is an outlier in your benefit data
    in rows", error_rows))
  }

  df_risk <- df_outcome %>% dplyr::filter(outcome == "Risk")
  mean2 <- mean(df_risk$diff)
  std2 <- sd(df_risk$diff)
  rmin <- mean2 - (3 * std2)
  rmax <- mean2 + (3 * std2)
  error_rows <- which(!(df_risk$diff > rmin) | !(df_risk$diff < rmax))
  if (!all(df_risk$diff > rmin) && !all(df_risk$diff < rmax)) {
    stop(paste("Custom error message: there is an outlier in your risk data
               in rows", error_rows))
  }

  stopifnot(all(df_outcome$outcome == "Benefit" | df_outcome$outcome == "Risk"))

  obsv_dur <- unique(df_outcome$obsv_duration)
  min1 <- min((df_outcome$diff) * subjects)
  num_break <- ifelse(((min1 * (-2)) %% 6 == 0), 6,
    ifelse(((min1 * (-2)) %% 8 == 0), 8,
      ifelse(((min1 * (-2)) %% 7 == 0), 7, 9)
    )
  )

  actual_min <- (min1 %% (-num_break)) + (min1)
  max1 <- max((df_outcome$diff) * subjects)

  breaks2 <- pretty(range(actual_min, max1), n = num_break)
  mytitle <- cowplot::ggdraw() + cowplot::draw_label(
    titlename,
    fontface = "bold", size = 12
  )

  plot1 <- ggplot() +
    geom_line(
      aes(x = df_ben$eventtime, y = df_ben$diff * subjects, color = "Benefit")
    ) +
    geom_line(
      aes(x = df_risk$eventtime, y = df_risk$diff * subjects, color = "Risk")
    ) +
    scale_color_manual(
      name = "", values = c(fig_colors[1], fig_colors[2]),
      limits = c("Benefit", "Risk")
    ) +
    geom_hline(yintercept = 0, color = "grey") +
    coord_cartesian(ylim = c(actual_min, max1)) +
    scale_x_continuous(
      limits = c(0, obsv_dur),
      breaks = seq(0, obsv_dur, visits)
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~., breaks = breaks2),
      n.breaks = num_break
    ) +
    xlab(paste("Time in", df_outcome$obsv_unit[1])) +
    labs(
      y = NULL,
      title = paste0(ben_name, "\n(>0: Favours ", active, ")"),
      subtitle = paste0(risk_name, "\n(>0: Favours ", comparator, ")")
    ) +
    br_charts_theme() +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(color = fig_colors[1]),
      axis.text.y.right = element_text(color = fig_colors[2]),
      axis.line = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = legend_position,
      legend.justification = "left",
      legend.direction = "horizontal",
      plot.title = element_text(
        size = 10,
        face = "plain",
        colour = fig_colors[1],
        hjust = 0,
        vjust = -7,
        margin = ggplot2::margin(
          t = -1,
          unit = "pt"
        )
      ),
      plot.subtitle = element_text(
        colour = fig_colors[2], size = 10,
        hjust = 1
      ),
      plot.margin = margin(14.506, 30, 14.506, 14.506),
      axis.title.y = element_text(color = fig_colors[1]),
      axis.title.y.right = element_text(color = fig_colors[2])
    )
  if (!is.null(titlename)) {
    plot_grid(
      mytitle,
      plot1,
      ncol = 1,
      align = "v",
      axis = "l",
      rel_heights = c(0.1, 2)
    )
  } else {
    plot1
  }
}

#' Create a table that corresponds to the cumulative excess plot
#'
#' @param df_table A dataframe with 5 variables named the following:
#' 1) obsv_duration: A variable that specifies the duration of the
#' observational period (numerical).
#' 2) n: A vector containing a number of subjects who experienced an event
#' at a given time (numerical).
#' 3) effect: specifies between an active or control effect.
#' 4) outcome: specifies whether the an outcome should be classified as a
#' "Benefit" or "Risk" (this must have either "Benefit" or "Risk" as values).
#' 5) eff_code: 0 for control and 1 for active effect.
#' @param subjects A numerical input that specifies the baseline proportion
#' of subjects in the study (for example, "per 100 subjects")
#' @param visits A numerical input that is the length between observational
#' periods.
#' @param fig_colors Allows the user to change the colors of the table
#' (defaults are provided). Must be vector of length 2, with color corresponding
#' to benefit second and risk first.
#'
#' @return A table.
#' @export
#' @examples
#' gensurv_table(cumexcess, 100, 6)
gensurv_table <- function(df_table,
                          subjects,
                          visits,
                          fig_colors = c("#0571b0", "#ca0020")) {
  effect <- outcome <- visit <- y <- color_ctrl_var <- NULL
  eff_code <- eventtime <- obsv_duration <- NULL

  if (!is.null(df_table$eventtime)) {
    all_columns <- c(
      "obsv_duration", "n", "effect", "outcome", "eff_code",
      "eventtime"
    )
    nonexistent_columns <- setdiff(all_columns, colnames(df_table))
    if (length(nonexistent_columns) > 0) {
      error_message <- paste0("You are missing a required variable in your
                              dataframe:", nonexistent_columns)
      stop(error_message)
    } else {
      df_table <- df_table %>%
        select(obsv_duration, n, effect, outcome, eff_code, eventtime)
    }
  } else {
    all_columns <- c("obsv_duration", "n", "effect", "outcome", "eff_code")
    nonexistent_columns <- setdiff(all_columns, colnames(df_table))
    if (length(nonexistent_columns) > 0) {
      error_message <- paste0("You are missing a required variable in your
                              dataframe:", nonexistent_columns)
      stop(error_message)
    } else {
      df_table <- df_table %>%
        select(obsv_duration, n, effect, outcome, eff_code)
    }
  }

  active <- which(df_table$eff_code == 1)
  active1 <- df_table$effect[active[1]]
  comparator <- which(df_table$eff_code == 0)
  comparator1 <- df_table$effect[comparator[1]]

  if (any(is.na(df_table))) {
    miss_vars <- colnames(df_table)[colSums(is.na(df_table) > 0)]
    warning(paste(
      "you have a missing value in row(s)",
      which(rowSums(is.na(df_table)) > 0)
    ))
    df_table[miss_vars] <- lapply(df_table[miss_vars], function(x) {
      ifelse(is.na(x), "NA", x)
    })
  }

  df_table1 <- df_table %>%
    mutate(
      y = dplyr::case_when(
        eff_code == 0 & outcome == "Benefit" ~ 3,
        eff_code == 1 & outcome == "Benefit" ~ 4,
        eff_code == 1 & outcome == "Risk" ~ 2,
        eff_code == 0 & outcome == "Risk" ~ 1
      ),
      color_ctrl_var = dplyr::case_when(
        eff_code == 0 & outcome == "Benefit" ~ fig_colors[1],
        eff_code == 1 & outcome == "Benefit" ~ fig_colors[1],
        eff_code == 1 & outcome == "Risk" ~ fig_colors[2],
        eff_code == 0 & outcome == "Risk" ~ fig_colors[2]
      )
    ) %>%
    mutate(effect = forcats::fct_reorder(
      as.factor(paste(effect, outcome,
        sep = " "
      )), y,
      .na_rm = FALSE
    ))

  len <- df_table$obsv_duration[1]
  visit <- seq.default(0, len, by = visits)

  if (!is.null(df_table1$eventtime)) {
    df_table1 <- df_table1 %>%
      filter(eventtime %in% unlist(visit)) %>%
      mutate(visit = eventtime) %>%
      select(visit, n, effect, y, color_ctrl_var) %>%
      distinct()
  } else {
    df_table1$visit <- visit
    df_table1 <- df_table1 %>%
      select(visit, n, effect, y, color_ctrl_var)
  }

  geom_text_ctrl <- list(
    aes_string(
      x = "visit",
      label = "n",
      y = "y"
    ),
    color = df_table1$color_ctrl_var,
    family = "sans",
    size = 3
  )

  x_ctrl <- list(
    breaks = unique(df_table1$visit),
    labels = NULL
  )


  if (any(is.na(df_table))) {
    df_table3 <- na.omit(df_table1)
    df_table3 <- droplevels(df_table3)

    y_ctrl <- list(
      name = "",
      breaks = c(1, 2, 3, 4),
      limits = c(0, 4.5),
      labels = levels(df_table3$effect)
    )
  } else {
    y_ctrl <- list(
      name = "",
      breaks = c(1, 2, 3, 4),
      limits = c(0, 4.5),
      labels = levels(df_table1$effect)
    )
  }

  extra_code <- labs(titles = "Number With Event")
  geom_text_control <- do.call(geom_text, geom_text_ctrl)
  scale_x_control <- do.call(scale_x_continuous, x_ctrl)
  scale_y_control <- do.call(scale_y_continuous, y_ctrl)

  ggplot(data = df_table1) +
    geom_text_control +
    scale_x_control +
    scale_y_control +
    extra_code +
    labs(caption = paste(
      "Total number of subjects:", active1, "=",
      subjects,
      "and", comparator1, "=",
      subjects
    )) +
    br_charts_theme() +
    theme(
      plot.title = ggplot2::element_text(size = 8),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      plot.margin = margin(14.506, 30, 14.506, 14.506),
      plot.caption = element_text(hjust = 0.42),
      legend.position = "none"
    )
}

#' Combine the cumulative excess plot and corresponding table into one figure
#'
#' @param df_plot A dataframe with 6 variables named the following:
#' 1) eventtime: A vector of time points at which an event occurred.
#' 2) diff: A vector containing the difference in active and control effects.
#' 3) obsv_duration: A variable that specifies the duration of the
#' observational period (numerical).
#' 4) obsv_unit: A variable that specifies the unit for the duration of the
#' observational period (this is a non-numerical input).
#' 5) outcome: A vector containing whether the outcome is a "Benefit" or "Risk".
#' 6) eff_diff_lbl: A vector containing the label for effect difference.
#' @param df_table A dataframe with 5 variables named the following:
#' 1) obsv_duration: A variable that specifies the duration of the
#' observational period (numerical).
#' 2) n: A vector containing a number of subjects who experienced an event
#' at a given time (numerical).
#' 3) effect: specifies between an active or control effect.
#' 4) outcome: specifies whether the an outcome should be classified as a
#' "Benefit" or "Risk" (this must have either "Benefit" or "Risk" as values).
#' 5) eff_code: 0 for control and 1 for active effect.
#' @param subjects_pt A numerical input that specifies the baseline proportion
#' of subjects in the study.
#' @param visits_pt A numerical input that is the length between observational
#' periods.
#' @param fig_colors_pt Allows user to change the colors of the figure (defaults
#' are provided). Must be vector of length 2, with color corresponding to
#' benefit first and risk second.
#' @param titlename_p Allows user to change the documentation of title (default
#' is provided)
#' @param rel_heights_table Elements for fig vs table size.
#' @param ben_name_p Allows user to specify benefit of interest
#' (default is provided).
#' @param risk_name_p Allows user to specify risk of interest
#' (default is provided).
#' @param legend_position_p Allows user to specify legend position. Must be a
#' vector of length 2, with the first value corresponding to the position of the
#' legend relative to the x-axis, and the second corresponding to the position
#' of the legend relative to the y-axis (numeric).
#'
#' @return A combined cumulative excess plot and table.
#' @export
#' @examples
#' gensurv_combined(
#'   df_plot = cumexcess, subjects_pt = 100, visits_pt = 6,
#'   df_table = cumexcess, fig_colors_pt = colfun()$fig13_colors
#' )
#'
gensurv_combined <- function(df_plot,
                             df_table,
                             subjects_pt,
                             visits_pt,
                             fig_colors_pt = c("#0571b0", "#ca0020"),
                             titlename_p =
                               "Cumulative Excess # of Subjects w/ Events
                             (per 1000 Subjects)",
                             rel_heights_table = c(1, 0.2),
                             ben_name_p = "Primary Efficacy",
                             risk_name_p = "Recurring AE",
                             legend_position_p = c(-0.03, 1.15)) {
  if (!is.null(df_table$eventtime)) {
    all_columns <- c(
      "obsv_duration", "n", "effect", "outcome", "eff_code",
      "eventtime"
    )
    nonexistent_columns <- setdiff(all_columns, colnames(df_table))
    if (length(nonexistent_columns) > 0) {
      error_message <- paste0("You are missing a required variable in your
                              table dataframe:", nonexistent_columns)
      stop(error_message)
    }
  } else {
    all_columns <- c("obsv_duration", "n", "effect", "outcome", "eff_code")
    nonexistent_columns <- setdiff(all_columns, colnames(df_table))
    if (length(nonexistent_columns) > 0) {
      error_message <- paste0("You are missing a required variable in your
                              table dataframe:", nonexistent_columns)
      stop(error_message)
    }
  }

  all_columns <- c(
    "obsv_duration", "eventtime", "diff", "obsv_unit",
    "outcome", "eff_diff_lbl"
  )
  nonexistent_columns <- setdiff(all_columns, colnames(df_plot))
  if (length(nonexistent_columns) > 0) {
    error_message <- paste0("You are missing a required variable in your
                            plot dataframe:", nonexistent_columns)
    stop(error_message)
  }

  plot <- gensurv_plot(
    df_plot, subjects_pt, visits_pt,
    fig_colors = fig_colors_pt,
    ben_name = ben_name_p, risk_name = risk_name_p,
    legend_position = legend_position_p
  )
  table <- gensurv_table(
    df_table, subjects_pt, visits_pt,
    fig_colors = fig_colors_pt
  )
  fig_plot <- cowplot::plot_grid(
    cowplot::plot_grid(
      plot,
      table +
        ggplot2::theme(legend.position = "none"),
      ncol = 1,
      align = "v",
      axis = "b",
      rel_heights = rel_heights_table
    ),
    ncol = 1
  )
  mytitle <- cowplot::ggdraw() + cowplot::draw_label(
    titlename_p,
    fontface = "bold", size = 12
  )
  fig_plot_title <- cowplot::plot_grid(
    mytitle,
    fig_plot,
    ncol = 1,
    align = "v",
    axis = "l",
    rel_heights = c(0.2, 2)
  )
  return(fig_plot_title)
}

#' Simulate data (utilized for function tests)
#'
#' @param seed A numerical input or object that will set the standard for data
#' simulation.
#' @param n1 A numerical input or object that represents the desired total
#' population for the simulated data (for treatment group 1).
#' @param n2 A numerical input or object that represents the desired total
#' population for the simulated data (for treatment group 2).
#' @param obsv_duration A numerical input or object that is the maximum time
#' the observational period will span for the simulated data.
#' @param lambda1 A numerical input or object that is the first lambda that will
#'  be used in the data simulation (used as the first parameter for simulation).
#' @param lambda2 A numerical input or object that is the second lambda that
#' will be used in the data simulation (used as the second parameter
#' for simulation).
#' @param unit A non-numerical input that specifies the length of the
#' observational period ("Week", "Month", etc.).
#'
#' @return A simulated dataframe.
#' @export
#'
#' @examples
#' gensurv(111, 2000, 1000, 36, .005, .0048, "Weeks")
gensurv <- function(
    seed, n1, n2, obsv_duration, lambda1, lambda2, unit = "Months") {
  diff_sim <- NULL
  stopifnot(is.numeric(seed))
  stopifnot(is.numeric(n1))
  stopifnot(is.numeric(n2))
  stopifnot(is.numeric(obsv_duration))
  stopifnot(is.numeric(lambda1))
  stopifnot(is.numeric(lambda2))
  stopifnot(is.character(unit))
  set.seed(seed)
  sim1 <- simsurv(
    dist = "exponential",
    lambdas = lambda1,
    x = data.frame(id = 1:n1),
    maxt = obsv_duration
  )
  sim2 <- simsurv(
    dist = "exponential",
    lambdas = lambda2,
    x = data.frame(id = 1:n2),
    maxt = obsv_duration
  )

  eventtime_sim <- sort(unique(c(sim1$eventtime, sim2$eventtime)))
  df_sim <- data.frame(matrix(0, length(eventtime_sim), 2))
  names(df_sim) <- c("eventtime_sim", "diff_sim")
  df_sim$eventtime_sim <- eventtime_sim

  for (t in seq_along(df_sim$eventtime_sim)) {
    df_sim$diff_sim[t] <- sum(
      sim1$eventtime < eventtime_sim[t] & sim1$status == 1
    ) /
      n1 - sum(sim2$eventtime < eventtime_sim[t] & sim2$status == 1) / n2
  }
  df_sim %>%
    mutate(obsv_duration = obsv_duration, obsv_unit = unit) %>%
    rename(eventtime = eventtime_sim, diff = diff_sim)
}
