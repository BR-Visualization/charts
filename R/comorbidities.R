#' Codes to create comorbidities
#'
#' @param data example data for the manuscript is `comorbidities`
#'
#' @return a ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' fig4 <- figure4(data = comorbidities)
#' ggsave_custom("figure4.jpeg", imgpath = "/cloud/project/inst/img/",
#' inplot = fig4, device = jpeg)
#'
figure4 <- function(data) {

  # grouped bar chart
  data$Severity <- factor(data$Severity,
                          levels = c("Mild", "Moderate", "Severe"))

  fig4 <- ggplot(data, aes_string(fill = "Severity",
                                  y = "Prevalence", x = "Comorbidities")) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(
      color = "Severity"
    ) +
    guides(fill = guide_legend(title = "Severity:")) +
    scale_fill_manual(values = colfun()$fig4_colors)
  fig4
}
