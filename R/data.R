#' Example demography data used for Figure 2
#'
#' @format ## `demography`
#' A data frame with 48 rows and 5 columns:
#' \describe{
#'   \item{X}{Record ID number}
#'   \item{Age}{Age in years}
#'   \item{Type}{Diabetis Type A/B}
#'   \item{Gender}{Gender Male/Female}
#'   \item{Prevalence}{Prevalence}
#'
#' }
"demography"

#' Example comorbidities data used for Figure 4
#'
#' @name comorbidities
#' @format A data frame with 15 rows and 3 variables
#' \describe{
#'   \item{Comorbidities}{Comorbidities}
#'   \item{Severity}{Severity of comorbidities Mild/Moderate/Severe}
#'   \item{Prevalence}{Prevalence of comorbidities of each severity level}
#'
#' }
"comorbidities"

#' Example effects table data used for Figure 6 and Figure 7
#'
#' @name effects_table
#' @format A data frame with 24 rows and 51 variables
"effects_table"

#' Example effects table
#'
#' @name brdata
#' @format A data frame with 105 rows and 51 variables
"brdata"

#' Example cumulative excess plot data used for Figure 13
#'
#' @name cumexcess
#' @format A data frame with 880 rows and 9 columns:
#'   \describe{
#'   \item{eventtime}{Simulated event times}
#'   \item{diff}{Simulated difference in active/control effects}
#'   \item{obsv_duration}{Duration of observational period}
#'   \item{obsv_unit}{Unit length of observational period}
#'   \item{outcome}{Specifies Benefit/Risk}
#'   \item{eff_diff_lbl}{Label for effect difference}
#'   \item{n}{Number of subjects}
#'   \item{effect}{Specifies active/control effect}
#'   \item{eff_code}{0/1 depicting control/active effects}
#'
#'   }
"cumexcess"
