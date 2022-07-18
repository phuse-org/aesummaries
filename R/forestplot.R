#' Create a widget forest plot table with comparisons
#' Create a widget forest plot table with comparisons
#'
#' @param data the dataset to make the forestplot with
#' @param high_level_col The High Level Adverse Event Class (AEBODSYS)
#' @param low_level_col this column is the Low Level Adverse Event Class (AEDECOD)
#' @param group1_col Name of first group in comparison (e.g. "Treatmemt")
#' @param group2_col Name of second group in comparison (e.g. "Placebo")
#' @param numerator1_col Number of cases (numerator) for group 1
#' @param numerator2_col Number of cases (numerator) for group 2
#' @param denominator1_col Number of participants (denominator) for group 1
#' @param denominator2_col Number of participants (denominator) for group 2
#' @param test_col Type of test (e.g. "Odds Ratio")
#' @param result_col Result of Test
#' @param result_upper_col Upper Limit of Normal for Test Result
#' @param result_lower_col Lower Limit of Normal for Test Result
#' @param p_col P value for Test Result
#' @param width widget width (default 100%)
#' @param height widget height (default 100%)
#' @param elementId id of forestPlot element 
#' @param comparisonWidth width of comparison column (200)
#'
#' @import htmlwidgets
#'
#' @export
forestplot <- function(data = testdata,
                       high_level_col = "AEBODSYS",
                       low_level_col = "AEDECOD",
                       group1_col = "Group1",
                       group2_col = "Group2",
                       numerator1_col = "n1",
                       numerator2_col = "n2",
                       denominator1_col = "N1",
                       denominator2_col = "N2",
                       test_col = "Test",
                       result_col ="Res",
                       result_upper_col ="CI_Upper",
                       result_lower_col ="CI_Lower",
                       p_col = "Pvalue",
                       width = "100%",
                       height = NULL,
                       elementId = NULL,
                       comparisonWidth=200) {

  # forward options using x
  x = list(
    data = data,
    settings = list(
      high_level_col = high_level_col,
      low_level_col = low_level_col,
      group1_col = group1_col,
      group2_col = group2_col,
      numerator1_col = numerator1_col,
      numerator2_col = numerator2_col,
      denominator1_col = denominator1_col,
      denominator2_col = denominator2_col,
      test_col = test_col,
      result_col = result_col,
      result_upper_col = result_upper_col,
      result_lower_col = result_lower_col,
      p_col = p_col,
      comparisonWidth=comparisonWidth
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'forestplot',
    x,
    width = width,
    height = height,
    package = 'aesummaries',
    elementId = elementId
  )
}

#' Shiny bindings for forestplot
#'
#' Output and render functions for using forestplot within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a forestplot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name forestplot-shiny
#'
#' @export
forestplotOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'forestplot', width, height, package = 'aesummary')
}

#' @rdname forestplot-shiny
#' @export
renderForestplot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, forestplotOutput, env, quoted = TRUE)
}
