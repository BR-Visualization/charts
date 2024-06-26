#' Pyramid Chart
#'
#' @param data `dataframe` demography data
#' @param levelvar `Factor` two factor levels, one for each pyramid
#' @param groupvar `Factor` two factor levels, one for each side of a pyramid
#' @param alpha_set `Value` specify transparency of symbols
#' @param xvar `value` x-axis
#' @param yvar `value` y-axis
#' @param chartcolors `vector` two colors, one for each side of a pyramid
#' @param xlab `text` for x-axis label
#'
#' @importFrom patchwork plot_layout
#' @importFrom dplyr mutate filter
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' demography |>
#'   dplyr::mutate(
#'     Type = as.factor(paste0("Type ", Type)),
#'     figprev = ifelse(
#'       Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000
#'     ),
#'     Sex = Gender
#'   ) |>
#'   pyramid_chart(
#'     levelvar = "Type", xvar = "figprev", yvar = "Age",
#'     groupvar = "Sex", alpha_set = 0.7, chartcolors = colfun()$fig2_colors,
#'     xlab = "Prevalence (x 100 000)"
#'   )
pyramid_chart <-
  function(data,
           xvar,
           yvar,
           levelvar,
           groupvar,
           xlab,
           alpha_set,
           chartcolors) {
    data_filtered <- data[!is.na(data[[xvar]]), ] # Filter out NA values
    figlimits <- c(-1, 1) * ceiling(max(abs(data_filtered[[xvar]])) / 10) * 10

    # Convert levelvar to factor or character if not already
    if (is.numeric(data_filtered[[levelvar]]) |
      is.character(data_filtered[[levelvar]])) {
      data_filtered[[levelvar]] <- as.factor(data_filtered[[levelvar]])
    } else if (!is.factor(data_filtered[[levelvar]]) &&
      !is.character(data_filtered[[levelvar]])) {
      stop("levelvar should be a factor, character, or numeric vector")
    }

    scale_x <- scale_x_continuous(
      limits = ~figlimits,
      breaks = seq(figlimits[1], figlimits[2], 10),
      labels = abs(seq(figlimits[1], figlimits[2], 10))
    )

    fig2_1 <- data_filtered |>
      filter(.data[[levelvar]] == levels(.data[[levelvar]])[[1]]) |>
      ggplot(aes(
        x = .data[[xvar]],
        y = .data[[yvar]],
        fill = .data[[groupvar]],
        color = .data[[groupvar]]
      )) +
      geom_col(alpha = alpha_set) +
      scale_x +
      scale_y_discrete() +
      scale_color_manual(values = chartcolors) +
      scale_fill_manual(values = chartcolors) +
      labs(
        title = levels(data[[levelvar]])[[1]],
        x = xlab
      ) +
      guides(
        fill = guide_legend(title = paste0(groupvar, ":")),
        color = guide_legend(title = paste0(groupvar, ":"))
      ) +
      br_charts_theme(
        axis.ticks = element_blank(),
        plot.margin = margin(4.5, 0, 4.5, 4.5),
        axis_line = element_blank()
      )

    fig2_2 <- data_filtered |>
      filter(.data[[levelvar]] == levels(.data[[levelvar]])[[2]]) |>
      ggplot(aes(
        x = .data[[xvar]],
        y = .data[[yvar]],
        fill = .data[[groupvar]],
        color = .data[[groupvar]]
      )) +
      geom_col(alpha = alpha_set) +
      scale_x +
      scale_y_discrete() +
      scale_color_manual(values = chartcolors) +
      scale_fill_manual(values = chartcolors) +
      guides(
        fill = guide_legend(title = paste0(groupvar, ":")),
        color = guide_legend(title = paste0(groupvar, ":"))
      ) +
      labs(
        title = levels(data[[levelvar]])[[2]], x = xlab
      ) +
      br_charts_theme(
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(4.5, 4.5, 4.5, 4.5),
        axis_line = element_blank()
      )

    cplot <- fig2_1 + theme(
      axis.text.y.left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
      fig2_2 + theme(
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) + plot_layout(guides = "collect") & theme(legend.position = "top")

    cplot
  }
