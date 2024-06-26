#' CI for absolute risk for binary outcomes
#'
#' Derive mean difference and associated confidence intervals
#' for binary outcomes
#'
#' @param prop1 (`numeric`)\cr Proportion of cases in active treatment
#' @param prop2 (`numeric`)\cr Proportion of cases in comparator treatment
#' @param N1 (`numeric`)\cr Total number of subjects in active treatment
#' @param N2 (`numeric`)\cr Total number of subjects in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' calculate_diff_bin(
#'   prop1 = .45, prop2 = 0.25, N1 = 500, N2 = 500,
#'   cl = 0.95
#' )
calculate_diff_bin <- function(prop1, prop2, N1, N2, cl) {
  zscore <- qnorm(0.5 + cl / 2)
  # Calculating the different elements
  diff <- prop1 - prop2
  se <- sqrt((1 - prop1) * prop1 / N1 + (1 - prop2) * prop2 / N2)
  lower <- diff - zscore * se
  upper <- diff + zscore * se

  # Creating a dataframe from the different calculated elements
  df <- data.frame(diff, se, lower, upper)

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] >
                absolute risk CI for binary outcomes is calculated and saved'))

  # Returning the df object
  df
}

#' CI for log relative risk for binary outcomes
#'
#' Derive log relative risk and associated confidence intervals
#' for binary outcomes
#'
#' @param prop1 (`numeric`)\cr Proportion of cases in active treatment
#' @param prop2 (`numeric`)\cr Proportion of cases in comparator treatment
#' @param N1 (`numeric`)\cr Total number of subjects in active treatment
#' @param N2 (`numeric`)\cr Total number of subjects in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @export
#'
#' @examples
#' calculate_log_rel_risk_bin(
#'   prop1 = .45, prop2 = 0.25, N1 = 500, N2 = 500,
#'   cl = 0.95
#' )
calculate_log_rel_risk_bin <- function(prop1, prop2, N1, N2, cl) {
  zscore <- qnorm(0.5 + cl / 2)
  # Calculating the different elements
  diff <- log(prop1 / prop2)
  se <- sqrt((1 - prop1) / prop1 / N1 + (1 - prop2) / prop2 / N2)
  lower <- diff - zscore * se
  upper <- diff + zscore * se

  diff[!is.finite(diff)] <- NA
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  # Creating a dataframe from the different calculated elements
  df <- data.frame(diff, se, lower, upper)

  # Writing a message that will be displayed in the log

  message(glue('[{format(Sys.time(),"%F %T")}] >
               log relative risk CI for binary outcomes is calculated
               and saved'))

  # Returning the df object
  df
}

#' CI for relative risk for binary outcomes
#'
#' Derive relative risk and associated confidence intervals
#' for binary outcomes
#'
#' @param prop1 (`numeric`)\cr Proportion of cases in active treatment
#' @param prop2 (`numeric`)\cr Proportion of cases in comparator treatment
#' @param N1 (`numeric`)\cr Total number of subjects in active treatment
#' @param N2 (`numeric`)\cr Total number of subjects in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @export
#'
#' @examples
#' calculate_rel_risk_bin(
#'   prop1 = .45, prop2 = 0.25, N1 = 500, N2 = 500,
#'   cl = 0.95
#' )
calculate_rel_risk_bin <- function(prop1, prop2, N1, N2, cl) {
  zscore <- qnorm(0.5 + cl / 2)
  # Calculating the different elements
  validate(need(
    prop2 != 0,
    "error : Proportion of cases in comparator treatment equal to 0"
  ))
  rr <- prop1 / prop2

  se <- sqrt((1 - prop1) / prop1 / N1 + (1 - prop2) / prop2 / N2)
  lower <- exp(log(rr) - zscore * se)
  upper <- exp(log(rr) + zscore * se)

  rr[!is.finite(rr)] <- NA
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  # Creating a dataframe from the different calculated elements
  df <- data.frame(rr, se, lower, upper)

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] >
               CI for relative risk for binary outcomes is calculated'))

  # Returning the df object
  df
}

#' CI for log odds ratio for binary outcomes
#'
#' Derive log odds ratio and associated confidence intervals
#' for binary outcomes
#'
#' @param prop1 (`numeric`)\cr Proportion of cases in active treatment
#' @param prop2 (`numeric`)\cr Proportion of cases in comparator treatment
#' @param N1 (`numeric`)\cr Total number of subjects in active treatment
#' @param N2 (`numeric`)\cr Total number of subjects in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @export
#'
#' @examples
#' calculate_log_odds_ratio_bin(
#'   prop1 = .45, prop2 = 0.25, N1 = 500, N2 = 500,
#'   cl = 0.95
#' )
calculate_log_odds_ratio_bin <- function(prop1, prop2, N1, N2, cl) {
  zscore <- qnorm(0.5 + cl / 2)
  # Calculating the different elements
  diff <- log((prop1 * (1 - prop2)) / (prop2 * (1 - prop1)))
  se <- sqrt(1 / (N1 * prop1) +
    1 / (N1 * (1 - prop1)) +
    1 / (N2 * prop2) +
    1 / (N2 * (1 - prop2)))
  lower <- diff - zscore * se
  upper <- diff + zscore * se

  diff[!is.finite(diff)] <- NA
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  # Creating a dataframe from the different calculated elements
  df <- data.frame(diff, se, lower, upper)

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] >
               log odds ratio CI for binary outcomes is calculated and saved'))

  # Returning the df object
  df
}

#' CI for odds ratio for binary outcomes
#'
#' Derive odds ratio and associated confidence intervals
#' for binary outcomes
#'
#' @param prop1 (`numeric`)\cr Proportion of cases in active treatment
#' @param prop2 (`numeric`)\cr Proportion of cases in comparator treatment
#' @param N1 (`numeric`)\cr Total number of subjects in active treatment
#' @param N2 (`numeric`)\cr Total number of subjects in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @export
#'
#' @examples
#' calculate_odds_ratio_bin(
#'   prop1 = .45, prop2 = 0.25, N1 = 500, N2 = 500,
#'   cl = 0.95
#' )
calculate_odds_ratio_bin <- function(prop1, prop2, N1, N2, cl) {
  zscore <- qnorm(0.5 + cl / 2)
  # Calculating the different elements
  validate(need(
    prop2 != 0,
    "error : Proportion of cases in comparator treatment equal to 0"
  ))
  or <- (prop1 * (1 - prop2)) / (prop2 * (1 - prop1))

  se <- sqrt(1 / (N1 * prop1) +
    1 / (N1 * (1 - prop1)) +
    1 / (N2 * prop2) +
    1 / (N2 * (1 - prop2)))
  lower <- exp(log(or) - zscore * se)
  upper <- exp(log(or) + zscore * se)

  or[!is.finite(or)] <- NA
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  # Creating a dataframe from the different calculated elements
  df <- data.frame(or, se, lower, upper)

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] >
               CI for odds ratio for binary outcomes is calculated and saved'))

  # Returning the df object
  df
}

#' CI for treatment difference in continuous outcomes
#'
#' Derive mean difference and associated confidence intervals
#' for continuous outcomes
#'
#' @param mean1 (`numeric`)\cr Mean of measure in active treatment
#' @param mean2 (`numeric`)\cr Mean of measure in comparator treatment
#' @param sd1 (`numeric`)\cr Standard deviation of measure in active treatment
#' @param sd2 (`numeric`)\cr Standard deviation of measure in comparator
#' treatment
#' @param N1 (`numeric`)\cr Total number of subjects in active treatment
#' @param N2 (`numeric`)\cr Total number of subjects in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @importFrom stats qt
#' @export
#'
#' @examples
#' calculate_diff_con(
#'   mean1 = 0.6, mean2 = 0.5, sd1 = 0.1, sd2 = 0.3,
#'   N1 = 400, N2 = 500, cl = 0.95
#' )
calculate_diff_con <- function(mean1, mean2, sd1, sd2, N1, N2, cl) {
  tscore <- qt(0.5 + cl / 2, N1 + N2 - 2)
  # Calculating the different elements
  diff <- mean1 - mean2
  sp2 <- ((N1 - 1) * sd1^2 + (N2 - 1) * sd2^2) / (N1 + N2 - 2)
  se <- sqrt(sp2 / N1 + sp2 / N2)
  lower <- diff - tscore * se
  upper <- diff + tscore * se

  # Creating a dataframe from the different calculated elements
  df <- data.frame(diff, se, lower, upper)

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] >
               CI for treatment difference in continuous outcomes is calculated'))

  # Returning the df object
  df
}

#' CI for treatment difference in exposure-adjusted rates
#'
#' Derive mean difference and associated confidence intervals
#' for exposure-adjusted rates (per 100 PYs)
#'
#' @param rate1 (`numeric`)\cr Event or incidence rate (per 100 PYs) in active treatment
#' @param rate2 (`numeric`)\cr Event or incidence rate (per 100 PYs) in comparator treatment
#' @param py1 (`numeric`)\cr 100PEY or 100PYAR in active treatment
#' @param py2 (`numeric`)\cr 100PEY or 100PYAR in comparator treatment
#' @param cl (`numeric`)\cr confidence level
#'
#' @export
#'
#' @examples
#' calculate_diff_rates(
#'   rate1 = 152.17, rate2 = 65.21, py1 = 230, py2 = 230,
#'   cl = 0.95
#' )
calculate_diff_rates <- function(rate1, rate2, py1, py2, cl) {
  zscore <- qnorm(0.5 + cl / 2)
  # Calculating the different elements
  diff <- rate1 - rate2
  se <- sqrt(rate1 / py1 + rate2 / py2)
  lower <- diff - zscore * se
  upper <- diff + zscore * se

  # Creating a dataframe from the different calculated elements
  df <- data.frame(diff, se, lower, upper)

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] >
               CI for treatment difference in exposure-adjusted rates
               is calculated and saved in a dataframe'))

  # Returning the df object
  df
}
