#' Demography Figure
#'
#' @param fig2_data `dataframe` demography data
#' @param Typev `Factor` Type of Diabetis A or B
#' @param gendervar `Factor` Gender variable
#' @param alpha_set `Value` specify transparency of symbols
#'
#' @importFrom patchwork plot_layout
#' @importFrom dplyr mutate filter
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(charts)
#' demography |>
#'   dplyr::mutate(
#'     figprev = ifelse(
#'       Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000
#'     ),
#'     Sex = Gender
#'   ) |>
#'   pyramid_chart(Typev = c("A", "B"), gendervar = "Sex", alpha_set = 0.7)
pyramid_chart <-
  function(fig2_data,
           Typev,
           gendervar,
           alpha_set) {
    figlimits <-
      c(-1, 1) * ceiling(max(abs(fig2_data[["figprev"]])) / 10) * 10

    scale_x <- scale_x_continuous(
      limits = ~figlimits,
      breaks = seq(figlimits[1], figlimits[2], 10),
      labels = abs(seq(figlimits[1], figlimits[2], 10))
    )

    fig2_1 <- fig2_data |>
      filter(.data[["Type"]] == Typev[[1]]) |>
      ggplot(aes(
        x = .data[["figprev"]],
        y = .data[["Age"]],
        fill = .data[[gendervar]],
        color = .data[[gendervar]]
      )) +
      geom_col(alpha = alpha_set) +
      scale_x +
      scale_y_discrete() +
      scale_color_manual(values = colfun()$fig2_colors) +
      scale_fill_manual(values = colfun()$fig2_colors) +
      labs(title = paste0("Type ", Typev[[1]]), x = "Prevalence (x 100 000)") +
      guides(
        fill = guide_legend(title = paste0({{ gendervar }}, ":")),
        color = guide_legend(title = paste0({{ gendervar }}, ":"))
      ) +
      charts::charts_style_theme(
        axis.ticks = element_blank(),
        plot.margin = margin(4.5, 0, 4.5, 4.5),
        axis_line = element_blank()
      )

    fig2_2 <- fig2_data |>
      filter(.data[["Type"]] == Typev[[2]]) |>
      ggplot(aes(
        x = .data[["figprev"]],
        y = .data[["Age"]],
        fill = .data[[gendervar]],
        color = .data[[gendervar]]
      )) +
      geom_col(alpha = alpha_set) +
      scale_x +
      scale_y_discrete() +
      scale_color_manual(values = colfun()$fig2_colors) +
      scale_fill_manual(values = colfun()$fig2_colors) +
      guides(
        fill = guide_legend(title = paste0({{ gendervar }}, ":")),
        color = guide_legend(title = paste0({{ gendervar }}, ":"))
      ) +
      labs(title = paste0("Type ", Typev[[2]]), x = "Prevalence (x 100 000)") +
      charts::charts_style_theme(
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
