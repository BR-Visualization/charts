#' Function for colors
#'
#' @return figure colors
#' @import colorBlindness
#' @export
#'
colfun <- function() {
  fig2_colors <- c("#00AFBB", "#FFDB6D")
  fig3_colors <- colorBlindness::Blue2DarkOrange18Steps[12:18]
  fig4_colors <- c("#ABDDA4", "#66C2A5", "#3288BD")
  fig6_colors <- c("#d7191c", "#009292", "#ff6db6", "#490092", "#006ddb")
  fig7_colors <- c("#009292", "#ff6db6", "#490092", "#006ddb")
  fig10_colors <- c("#0571b0", "white", "#ca0020")
  fig11_colors <- c("#00AFBB", "red", "blue")
  fig12_colors <- c("#0571b0", "#92c5de", "#f7f7f7", "#f4a582", "#ca0020")
  fig13_colors <- c("#0571b0", "#ca0020")

  return(list(
    fig2_colors = fig2_colors,
    fig3_colors = fig3_colors,
    fig4_colors = fig4_colors,
    fig6_colors = fig6_colors,
    fig7_colors = fig7_colors,
    fig10_colors = fig10_colors,
    fig11_colors = fig11_colors,
    fig12_colors = fig12_colors,
    fig13_colors = fig13_colors
  ))
}

#' Prepare data analysis for binary and continuous outcomes with Supplied
#' interval confidence
#' identifies whether the dataframe is for Benefit or Risk analysis
#' @param df (`data.frame`) dataset
#' either `df_benefit` (selected benefit)
#' or `df_risk` (select risk).
#' @param colname (`character`) feature to fetch for the analysis
#' either `Mean`, `Prop`, `Rate`
#' @param metric_name (`character`) metric for which we must fetch the
#' confidence interval if supplied (taken from the effect table)
#' either `Diff`, `RelRisk`, `OddsRatio`, `Diff_Rates`
#' @param func (`function`) function used to calculate metrics (or BR points)
#' @return data frame for specified type of analysis
#' @details DETAILS
#' @rdname prepare_br_supplied_ci
#' @export


prepare_br_supplied_ci <- function(df, colname, metric_name, func) {
  outcome <- sub(".*_", "", deparse(substitute(df)))
  output <- data.frame(
    df$Type,
    df$Category,
    df$Trt1,
    func(df[paste0(colname, "1")], df[paste0(colname, "2")]),
    df[paste0(metric_name, "_LowerCI")],
    df[paste0(metric_name, "_UpperCI")]
  )
  names(output) <- c(
    paste0(outcome, "_Type"),
    "Category",
    "Trt1",
    outcome,
    paste0(outcome, "_lowerCI"),
    paste0(outcome, "_upperCI")
  )
  output
}

#' Prepare data analysis for binary and continuous outcomes with Calculated
#' interval confidence
#' identifies whether the dataframe is for Benefit or Risk analysis
#' @param df (`data.frame`) dataset
#' either `df_benefit` (selected benefit)
#' or `df_risk` (select risk).
#' @param colname1 (`character`) feature to fetch for the analysis
#' either `Mean`, `Prop`, `Rate`
#' @param colname2 (`character`) feature to fetch for the analysis
#' either `nPat`, `Py`
#' @param func (`function`) function used to calculate metrics (or BR points)
#' @return data frame for specified type of analysis
#' @details DETAILS
#' @rdname prepare_br_calculated_ci
#' @export



prepare_br_calculated_ci <- function(df, colname1, colname2, func) {
  outcome <- sub(".*_", "", deparse(substitute(df)))
  output <- data.frame(
    df$Type,
    df$Category,
    df$Trt1,
    func(
      as.vector(unlist(df[paste0(colname1, "1")])),
      as.vector(unlist(df[paste0(colname1, "2")])),
      as.vector(unlist(df[paste0(colname2, "1")])),
      as.vector(unlist(df[paste0(colname2, "2")]))
    )
  )

  names(output) <- c(
    paste0(outcome, "_Type"),
    "Category",
    "Trt1",
    outcome,
    "se",
    paste0(outcome, "_lowerCI"),
    paste0(outcome, "_upperCI")
  )
  output
}

#' Partially bold a string
#' @param ... input argument for list function
#'
#' @return Expression
#' @export
#'
#' @examples
#' add_exprs("test_bold)", "not bold")
add_exprs <- function(...) {
  x <- list(...)
  Reduce(function(a, b) bquote(bold(.(a)):.(b)), x)
}

#' Create expression
#'
#' Selectively bold label for [ggplot2::ggplot2()].
#'
#' @param cond (`numeric`) expected conditional variable bold(1), not to bold(0)
#' @param bold (`character`)  level to bold
#' @param nonbold (`character`)\cr which level to bold.
#'
#' @details The function bold text in variable (`bold`) and concatenates it
#' with string in (`nonbold`) and returns a `dataframe`.
#'
#' @import magrittr dplyr
#' @importFrom glue glue
#'
#' @seealso `?plotmath`.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' xxx <- tribble(
#'   ~x, ~z, ~w, ~y,
#'   1, "BOLD_AA", " plain", 1,
#'   2, "b", "b", 0,
#'   3, "c", "c", 0
#' )
#' ggplot(xxx, aes_string(x = "x", y = "z")) +
#'   geom_point() +
#'   scale_y_discrete(
#'     label = labs_bold(cond = xxx[["y"]], xxx[["z"]], nonbold = xxx[["w"]])
#'   )
#'
labs_bold <- function(cond, bold, nonbold) {
  gout <- vector("expression", length(bold))

  for (i in seq_along(bold)) {
    if (cond[[i]] == 1) {
      gout[[i]] <- add_exprs(bold[[i]], nonbold[[i]])
    } else {
      gout[[i]] <- nonbold[[i]]
    }
  }

  # Writing a message that will be displayed in the log
  message(glue('[{format(Sys.time(),"%F %T")}] > Dataout object from
               the labs_bold function is created'))

  # Returning the dataout object
  gout
}

#' Derive minimum boundary value for axis
#' Derive boundary value to include all values
#'
#' @param rmin (`numeric`) number to evaluate
#' @param type_scale (`character`) selected scale display type
#' @return numeric
#' @export
#'
#' @examples
#' relmin(0.5, "Free")
#' relmin(0.5, "Fixed")
#' relmin(-0.3, "Free")
#' relmin(-0.3, "Fixed")
relmin <- function(rmin, type_scale) {
  if (type_scale == "Fixed") {
    ifelse(rmin >= 0,
      0,
      ifelse(
        rmin >= -1,
        floor(10 * rmin) / 10,
        floor(rmin)
      )
    )
  } else {
    ifelse(rmin >= 1,
      floor(rmin),
      ifelse(rmin >= -1,
        floor(10 * rmin) / 10,
        floor(rmin)
      )
    )
  }
}

#' Derive maximum boundary value for axis
#' Derive boundary value to include all values
#'
#' @param rmax (`numeric`) number to evaluate
#' @param type_scale (`character`) selected scale display type
#' @return numeric
#' @export
#'
#' @examples
#' relmax(0.5, "Free")
#' relmax(0.5, "Fixed")
#' relmax(-0.3, "Free")
#' relmax(-0.3, "Fixed")
relmax <- function(rmax, type_scale) {
  if (type_scale == "Fixed") {
    ifelse(rmax <= 0,
      0,
      ifelse(
        rmax <= 1,
        ceiling(10 * rmax) / 10,
        ceiling(rmax)
      )
    )
  } else {
    ifelse(rmax <= -1,
      ceiling(rmax),
      ifelse(rmax <= 1,
        ceiling(10 * rmax) / 10,
        ceiling(rmax)
      )
    )
  }
}

#' Wrapper to ggsave: Save a ggplot (or other grid object) with sensible
#' defaults
#'
#' Adds customized defaults to ggsave for the BRAP Journal requirements
#'
#' @param save_name File name to create on disk.
#' @param inplot 	Plot to save, defaults to last plot displayed.
#' @param imgpath Path of the directory to save plot to: path
#' @param bgcol Background color. If NULL, uses the plot.background fill value
#' from the plot theme.
#' @param ... Other arguments passed on to the graphics device function,
#' as specified by device.
#' @param bgcol Background color. If NULL, uses the plot.background fill value
#' from the plot theme.
#' @param wdth width of plot
#' @param hght height of plot
#' @param unts units of plot
#' @param create_dir create directory of output if folder doesn't exist
#'
#' @export
#'
#' @examples
#' fig4 <- figure4(data = comorbidities)
#' ggsave_custom("figure4.jpeg", imgpath = "inst/img/", inplot = fig4)
ggsave_custom <-
  function(save_name,
           inplot,
           wdth = 7,
           hght = 4.1,
           unts = "in",
           imgpath = "inst/img/",
           bgcol = "white",
           ...) {
    ggsave(
      filename = paste0(imgpath, save_name),
      plot = inplot,
      width = wdth,
      height = hght,
      units = unts,
      bg = bgcol,
      ...
    )
  }
