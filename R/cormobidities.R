#' Codes to create cormobidities
#'
#' @param data example data for the manuscript is `comorbidities`
#'
#' @return a ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' cormobidities <- cormobidities(data = comorbidities)
#' ggsave_custom("cormobidities.jpeg", imgpath = "~/charts/inst/img/",
#' inplot = cormobidities, device = jpeg)
#'
cormobidities <- function(data) {

  # grouped bar chart
  data$Severity <- factor(data$Severity,
                          levels = c("Mild", "Moderate", "Severe"))

  cormobidities <- ggplot(data, aes_string(fill = "Severity",
                                  y = "Prevalence", x = "Comorbidities")) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(
      color = "Severity"
    ) +
    guides(fill = guide_legend(title = "Severity:")) +
    scale_fill_manual(values = cormobidities_colors)
  cormobidities
}
