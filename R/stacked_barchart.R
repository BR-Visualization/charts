#' Stacked Bar Chart
#'
#' @param data `dataframe` input data
#' @param chartcolors `vector` a vector of colors, the same number of levels as
#' the brcat variable
#' @param xlabel `character` x label name, default is "Visit"
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot1 <- stacked_barchart(
#'   data = comp_outcome,
#'   chartcolors = colfun()$fig12_colors,
#'   xlabel = "Study Week"
#'  )
#' ggsave(plot1, filename = "tests/testthat/_snaps/stacked_barchart1.png", width = 7, height = 5)
#'
#' # unequal number of subjects across treatments
#' comp_outcome2 <- comp_outcome[
#'   (comp_outcome$trtn == 1 & comp_outcome$usubjid %in% c(1:40))
#'   | (comp_outcome$trtn == 2 & comp_outcome$usubjid %in% c(101:160))
#'   | (comp_outcome$trtn == 3 & comp_outcome$usubjid %in% c(201:250))
#' ,]
#' plot2 <- stacked_barchart(
#'   data = comp_outcome2,
#'   chartcolors = colfun()$fig12_colors,
#'   xlabel = "Study Week"
#'  )
#' ggsave(plot2, filename = "tests/testthat/_snaps/stacked_barchart2.png", width = 7, height = 5)
#'
#' # unequal number of observations across visits
#' comp_outcome3 <- comp_outcome[!(comp_outcome$trtn == 1 &
#'   comp_outcome$usubjid %in% c(1:40) & comp_outcome$visit == 5), ]
#' plot3 <- stacked_barchart(
#'   data = comp_outcome3,
#'   chartcolors = colfun()$fig12_colors,
#'   xlabel = "Study Week"
#'  )
#' ggsave(plot3, filename = "tests/testthat/_snaps/stacked_barchart3.png", width = 7, height = 5)

stacked_barchart <- function(data, chartcolors, xlabel = "Visit"){
  all_columns <- c(
    "usubjid", "visit", "trt", "brcat"
  )
  nonexistent_columns <- setdiff(all_columns, colnames(data))

  if (length(nonexistent_columns) > 0) {
    error_message <- paste0("You are missing a required variable in your dataframe: ", nonexistent_columns)
    stop(error_message)
  }

  df_n1 <- data %>%
    group_by(trt, visit) %>%
    summarise(n1 = n())

  if (nrow(unique(df_n1[c("trt","n1")])) > nrow(unique(df_n1["trt"]))) {
    warning(paste(
      "You have unequal number of observations across visits, please check missing data."
    ))
  }

  df_n2 <- data %>%
    group_by(trt, visit, brcat) %>%
    summarise(n2=n())

  df_stacked <- merge(df_n1, df_n2, by = c("trt", "visit"))
  df_stacked$percentage <- df_stacked$n2 / df_stacked$n1 * 100

  fig <- ggplot(df_stacked, aes(x = visit, y = percentage, fill = brcat)) +
    facet_wrap(~trt, scales = "free_x") +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values = chartcolors) +
    geom_text(aes(label = round(percentage, 0)), vjust = 1.3, position = "Stack",
              size = control_fonts()$p * 0.35) +
    scale_y_continuous(expand = c(0.015, 0)) +
    xlab(xlabel) +
    ylab("Percentage") +
    guides(fill = guide_legend(title="Outcome", nrow = 2, byrow = TRUE)) +
    labs(color = NULL) +
    br_charts_theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )

  fig
}
