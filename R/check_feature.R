#' Check if data contains required features to run a specific plot
#' @param df (`data.frame`) dataset - effect table
#' @return missing features
#' @details DETAILS
#' @rdname check_effects_tables
#' @importFrom shiny tags
#' @export
#'
#' @examples
#' check_effects_table(brdata)
check_effects_table <- function(df) {
  tags_ui <- paste0(
    "<ol>",
    check_feature(
      data = df, feature = "Factor",
      plots = c("value_tree", "forest", "contour", "tradeoff"),
      func = is.character, na_check = T,
      values = c("Benefit", "Risk")
    ),
    check_feature(
      data = df, feature = "Grouped_Outcome", plots = "value_tree",
      func = is.character, na_check = T
    ),
    check_feature(
      data = df, feature = "Outcome",
      plots = c("value_tree", "forest", "tradeoff", "contour"),
      func = is.character, na_check = T,
      check_unique = c(
        "Factor", "Grouped_Outcome",
        "Statistics", "Outcome_Status",
        "Type"
      )
    ),
    check_feature(
      data = df, feature = "Statistics", plots = "value_tree",
      func = is.character, na_check = T
    ),
    check_feature(
      data = df, feature = "Outcome_Status", plots = "value_tree",
      is.character, na_check = T,
      values = c("Identified", "Potential")
    ),
    check_feature(
      data = df, feature = "Filter",
      plots = c("forest", "contour", "tradeoff"),
      func = is.character, na_check = T
    ),
    check_feature(
      data = df, feature = "Category",
      plots = c("forest", "contour", "tradeoff"),
      func = is.character, na_check = T
    ),
    check_feature(
      data = df, feature = "Type",
      plots = c("contour", "forest", "tradeoff"),
      func = is.character, na_check = T,
      values = c("Binary", "Continuous")
    ),
    check_feature(
      data = df, feature = "Rate_Type",
      plots = c("contour", "forest", "tradeoff"),
      func = is.character
    ),
    check_feature(
      data = df, feature = "Mean1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric
    ),
    check_feature(
      data = df, feature = "Mean2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric
    ),
    check_feature(
      data = df, feature = "Prop1",
      plots = c("contour", "forest", "tradeoff"), func = is.numeric,
      check_range = c(0, 1)
    ),
    check_feature(
      data = df, feature = "Prop2",
      plots = c("contour", "forest", "tradeoff"), func = is.numeric,
      check_range = c(0, 1)
    ),
    check_feature(
      data = df, feature = "N1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.integer, check_positive = T
    ),
    check_feature(
      data = df, feature = "N2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.integer, check_positive = T
    ),
    check_feature(
      data = df, feature = "100PYAR1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "100PYAR2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "IncRate1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "IncRate2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "100PEY1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "100PEY2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "EventRate1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "EventRate2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "Sd1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "Sd2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.numeric, check_positive = T
    ),
    check_feature(
      data = df, feature = "Trt1",
      plots = c("contour", "forest", "tradeoff"),
      func = is.character,
      na_check = T
    ),
    check_feature(
      data = df, feature = "Trt2",
      plots = c("contour", "forest", "tradeoff"),
      func = is.character, na_check = T, check_same = T
    ),
    check_feature(
      data = df, feature = "Diff_LowerCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "Diff_UpperCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "Diff_IncRate_LowerCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "Diff_IncRate_UpperCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "Diff_EventRate_LowerCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "Diff_EventRate_UpperCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "RelRisk_LowerCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "RelRisk_UpperCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "OddsRatio_LowerCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "OddsRatio_UpperCI",
      plots = c("forest", "tradeoff"), func = is.numeric
    ),
    check_feature(
      data = df, feature = "Drug_Status", plots = "tradeoff",
      func = is.character, values = c("Approved", "Test")
    ),
    "</ol>"
  )
  tags_ui
}

#' intermediate function used to display log messages
#' check if a specific feature exist in the data
#' @param data dataset
#' @param feature (`data.frame`) feature to display analysis
#' @param plots (`function`) type of analysis (graph)
#' either `forest`, `tradeoff`, `contour`, `value_tree`
#' @param func function to check data type
#' @param na_check (`logical`) check if the feature has missing values
#' @param values (`vector`) check if the feature contains specified values
#' @param check_same (`logical`) check if the feature has the same value across all rows
#' @param check_range (`vector`) check if the feature is in the specified range
#' @param check_positive (`logical`) check if the feature is positive
#' @param check_unique (`vector`) check if unique values of a feature is associated with unique values of linked features
#' @return error message(s), if any
#' @details DETAILS
#' @rdname check_feature
#' @importFrom shiny tags
#' @importFrom stats na.omit
#' @export
#'
check_feature <- function(data, feature, plots, func, na_check, values,
                          check_same, check_range, check_positive,
                          check_unique) {
  error_msg <- ""
  # check if the feature is available in the effects table
  if (!(feature %in% colnames(data))) {
    error_msg <- glue(
      "<li><span>Feature <b>{feature}</b> is missing in the ",
      "effect table : errors might occur in <b>{toString(plots)}",
      "</b> plot(s)</span></li>"
    )
  }
  # check if the feature is not empty
  else if (sum(is.na(data[feature])) == nrow(data)) {
    error_msg <- glue(
      "<li><span>Feature <b>{feature}</b> is empty : errors ",
      "might occur in <b>{toString(plots)}</b> plot(s)</span></li>"
    )
  }
  # check if the feature has the right type
  else if (!func(data[[feature]])) {
    typestr <- gsub("is.|\\(\\)", "", deparse(substitute(func())))
    error_msg <- glue(
      "<li><span>Feature <b>{feature}</b> must be of type ",
      "{typestr} : errors might occur in <b>{toString(plots)}</b>",
      " plot(s)</span></li>"
    )
  } else {
    # check the content of the feature
    # check if the feature has missing values
    if (!missing(na_check) && na_check == T) {
      if (sum(is.na(data[feature])) > 0) {
        error_msg <- glue(
          "<li><span>Feature <b>{feature}</b> has missing values",
          " : errors might occur in ",
          "<b>{toString(plots)}</b> plot(s)</span></li>"
        )
      }
    }
    # check if the feature contains specified values
    if (!missing(values)) {
      if (!(all(data[[feature]] %in% values))) {
        error_msg <- paste(error_msg, glue(
          "<li><span>Feature <b>{feature}</b>",
          " must have the following values **{toString(values)}**",
          " : errors might occur in <b>{toString(plots)}</b> plot(s)",
          "</span></li>"
        ))
      }
    }
    if (!missing(check_same) && check_same == T) {
      if (!(length(unique(data[[feature]])) == 1)) {
        error_msg <- paste(error_msg, glue(
          "<li><span>Feature <b>{feature}</b>",
          " must  have the same value accross all rows : errors",
          " might occur in <b>{toString(plots)}</b> plot(s)</span>",
          "</li>"
        ))
      }
    }
    if (!missing(check_range)) {
      feature_with_no_na <- as.numeric(na.omit(data[[feature]]))
      b1 <- all(feature_with_no_na >= check_range[1])
      b2 <- all(feature_with_no_na <= check_range[2])
      if (!(b1 && b2)) {
        error_msg <- paste(error_msg, glue(
          "<li><span>Feature <b>{feature}",
          "</b> must be between [{toString(check_range)}]",
          " : errors might occur in <b>{toString(plots)}</b> plot(s)",
          "</span></li>"
        ))
      }
    }
    if (!missing(check_positive) && check_positive == T) {
      feature_with_no_na <- as.numeric(na.omit(data[[feature]]))
      if (!(all(feature_with_no_na >= 0))) {
        error_msg <- paste(error_msg, glue(
          "<li><span>Feature <b>{feature}",
          "</b> must be positive",
          " : errors might occur in <b>{toString(plots)}</b> plot(s)",
          "</span></li>"
        ))
      }
    }
    if (!missing(check_unique) && length(check_unique) > 0) {
      if (all(check_unique %in% names(data)) == TRUE) {
        result <- data %>%
          select(c(feature, check_unique)) %>%
          group_by(get(feature)) %>%
          summarise_all(n_distinct)

        if (!all(apply(
          result[, 2:ncol(result)], 2,
          function(a) length(unique(a)) == "1"
        ) == TRUE)) {
          error_msg <- paste(error_msg, glue(
            "<li><span>Feature ",
            "<b>{feature}</b> error : Each unique {feature} should be",
            " associated with unique <b>{toString(check_unique)}</b>",
            "  : errors occur in <b>{toString(plots)}</b> plot(s)",
            "</span></li>"
          ))
        }
      }
    }
  }

  error_msg
}

#' intermediate function used to display log messages
#' check if a specific feature exist in the data
#' @param data dataset
#' @param feature (`data.frame`) feature to display analysis
#' @param plots (`function`) type of analysis (graph)
#' either `forest`, `tradeoff`, `contour`, `value_tree`
#' @param func function to check data type
#' @param na_check (`logical`) check if the feature has missing values
#' @param values (`vector`) check if the feature contains specified values
#' @param check_same (`logical`) check if the feature has the same value across all rows
#' @param check_range (`vector`) check if the feature is in the specified range
#' @param check_positive (`logical`) check if the feature is positive
#' @param check_unique (`vector`) check if unique values of a feature is associated with unique values of linked features
#' @param add_msg (`character`) added error message for empty feature
#' @return error message(s), if any
#' @details DETAILS
#' @rdname check_feature_string
#' @importFrom shiny tags
#' @export
#'
check_feature_string <- function(data, feature, plots, func, na_check, values,
                                 check_same, check_range, check_positive,
                                 check_unique, add_msg = "") {
  error_msg <- ""
  # check if the feature is available in the effects table
  if (!(feature %in% colnames(data))) {
    error_msg <- glue(
      "Feature {feature} is missing in the ",
      "effect table : errors occur in {toString(plots)}",
      " plot(s);"
    )
  }
  # check if the feature is not empty
  else if (sum(is.na(data[feature])) == nrow(data)) {
    error_msg <- glue(
      "Feature {feature} is empty : errors ",
      "occur in {toString(plots)} plot(s); {add_msg}"
    )
  }
  # check if the feature has the right type
  else if (!func(data[[feature]])) {
    typestr <- gsub("is.|\\(\\)", "", deparse(substitute(func())))
    error_msg <- glue(
      "Feature {feature} must be of type ",
      "{typestr} : errors occur in {toString(plots)}",
      " plot(s);"
    )
  } else {
    # check the content of the feature
    # check if the feature has missing values
    if (!missing(na_check) && na_check == T) {
      if (sum(is.na(data[feature])) > 0) {
        error_msg <- glue(
          "Feature {feature} has missing values",
          " : errors occur in ",
          "{toString(plots)} plot(s);"
        )
      }
    }
    # check if the feature contains specified values
    if (!missing(values)) {
      if (!(all(data[[feature]] %in% values))) {
        error_msg <- paste(error_msg, glue(
          "Feature {feature}",
          " must have the following values **{toString(values)}**",
          " : errors occur in {toString(plots)} plot(s);"
        ))
      }
    }
    if (!missing(check_same) && check_same == T) {
      if (!(length(unique(data[[feature]])) == 1)) {
        error_msg <- paste(error_msg, glue(
          "Feature {feature}",
          " must have the same value across all rows : errors",
          " occur in {toString(plots)} plot(s);"
        ))
      }
    }
    if (!missing(check_range)) {
      feature_with_no_na <- as.numeric(na.omit(data[[feature]]))
      b1 <- all(feature_with_no_na >= check_range[1])
      b2 <- all(feature_with_no_na <= check_range[2])
      if (!(b1 && b2)) {
        error_msg <- paste(error_msg, glue(
          "Feature {feature}",
          " must be between [{toString(check_range)}]",
          " : errors occur in {toString(plots)} plot(s);"
        ))
      }
    }
    if (!missing(check_positive) && check_positive == T) {
      feature_with_no_na <- as.numeric(na.omit(data[[feature]]))
      if (!(all(feature_with_no_na >= 0))) {
        error_msg <- paste(error_msg, glue(
          "Feature {feature}",
          " must be positive",
          " : errors occur in {toString(plots)} plot(s);"
        ))
      }
    }
    if (!missing(check_unique) && length(check_unique) > 0) {
      if (all(check_unique %in% names(data)) == TRUE) {
        result <- data %>%
          select(c(feature, check_unique)) %>%
          group_by(get(feature)) %>%
          summarise_all(n_distinct)
        if (!all(apply(
          result[, 2:ncol(result)], 2,
          function(a) length(unique(a)) == "1"
        ) == TRUE)) {
          error_msg <- paste(error_msg, glue(
            "Feature {feature} error :",
            " Each unique {feature} should be associated with unique",
            " {toString(check_unique)} : errors occur",
            " in {toString(plots)} plot(s);"
          ))
        }
      }
    }
  }

  error_msg
}
