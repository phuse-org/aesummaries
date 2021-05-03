#' Create a widget forest plot table with comparisons
#'
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
                       width = NULL,
                       height = NULL,
                       elementId = NULL) {

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
      p_col = p_col
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
