## Transformation Function ###
#' Reverse Log transformation of value to pass to scale options
#'
#' @param base Logarithmic base value.
#'
#' @return Transformation object per given base
#' @export
#'
#' @examples
#' library(cvars)
#' library(ggplot2)
#' ggplot(data = mtcars, mapping = aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   scale_y_continuous(trans = reverselog_trans(10))
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
    log_breaks(base = base),
    domain = c(1e-100, Inf)
  )
}


#' Variable Labels and Corresponding names from dataframe
#'
#' @param datain Input dataframe to get variable labels for
#'
#' @return a dataframe with corresponding label for each variable
#'
#' @export
#'
#' @examples
#' library(cvars)
#' data("adae")
#' data_attrib(adae)
data_attrib <- function(datain) {
  # converting all the column names to upcase
  names(datain) <- toupper(names(datain))
  # Get variable names and labels into dataframe
  data_attr <- setNames(
    data.frame(
      names(datain),
      unlist(lapply(
        names(datain),
        function(x) {
          ifelse(is.null(attr(datain[[x]], "label")),
            x, attr(datain[[x]], "label")
          )
        }
      ))
    ),
    c("VAR_NAMES", "VAR_LABEL")
  )
  return(data_attr)
}


## Line/Marker/Bar/Box color per Series (Treatment) grouping:##
#' Set Colors to Series Variable to plot
#'
#' Assigns selected colors to levels of a series variable to be used for plotting and legend.
#'
#' @param gdata Data being used to create graph
#' @param series_color Comma-separated colors corresponding to values in SERIESVAR
#' @param SERIESVAR Series Variable to assign colors to. Examples: "TRTVAR","ETHNIC"
#' Preferably a variable of type factor with distinct levels
#'
#' @return named list of colors per level of grouping variable
#' @export
#'
#' @examples
#' data("ae_pre")
#' g_seriescol(ae_pre$dsin, "red,cyan,forestgreen,black", "TRTVAR")
g_seriescol <- function(gdata,
                        series_color = NA,
                        SERIESVAR) {
  if (!all(is.na(series_color))) {
    col_list <- unlist(str_split(series_color, ","))
  } else {
    # Standard Colors
    col_list <- c(
      "firebrick2", "blue4", "forestgreen", "gold", "magenta3",
      "aquamarine1", "tan4", "skyblue1", "orchid3", "brown", "pink", "black"
    )
  }

  # If not factor, convert it:
  if (!is.factor(gdata[[SERIESVAR]])) gdata[[SERIESVAR]] <- as.factor(gdata[[SERIESVAR]])

  # Set names as factor levels
  seriescols <- setNames(
    col_list[seq_along(unique(gdata[[SERIESVAR]]))],
    levels(unique(gdata[[SERIESVAR]]))
  )
  return(seriescols)
}

## Marker shapes grouped by Series (Treatment) groups:##
#' Set Shapes to Series Variable to plot
#'
#' Assigns selected point shapes to levels of a series variable to be used for plotting and legend.
#'
#' @param gdata Data being used to create graph
#' @param series_shapes Comma-separated shapes corresponding to values in SERIESVAR
#' @param SERIESVAR Series Variable to assign colors to. Examples: "TRTVAR","ETHNIC"
#' Preferably a variable of type factor with distinct levels
#'
#' @return named list of symbols per level of grouping variable
#' @export
#'
#' @examples
#' data(ae_pre)
#' g_seriessym(ae_pre$dsin, "triangle,cross,circle,square", "TRTVAR")
g_seriessym <- function(gdata,
                        series_shapes = NA,
                        SERIESVAR) {
  if (!all(is.na(series_shapes))) {
    if (is.numeric(series_shapes)) {
      shapelist <- series_shapes
    } else {
      shapelist <- unlist(str_split(series_shapes, ","))
      shapelist <- as.numeric(recode(shapelist,
        triangle = "2", trianglefilled = "24",
        square = "0", squarefilled = "22",
        circle = "1", circlefilled = "21",
        diamond = "5", diamondfilled = "23",
        inverttrianglefilled = "25", plus = "3",
        cross = "4", asterisk = "8"
      ))
    }
  } else {
    # Standard shapes
    shapelist <- c(16, 17, 15, 1, 18, 2, 0, 8, 10, 3, 4, 5)
  }

  # If not factor, convert it:
  if (!is.factor(gdata[[SERIESVAR]])) gdata[[SERIESVAR]] <- as.factor(gdata[[SERIESVAR]])

  # Set names as factor levels
  ptshapes <- setNames(
    shapelist[seq_along(unique(gdata[[SERIESVAR]]))],
    levels(unique(gdata[[SERIESVAR]]))
  )
  return(ptshapes)
}


## Identify any variable names starting with given string. Useful for Byvar,subgrp identification ##
#' Find column names with starting pattern
#'
#' Searches column names of given dataframe to identify which start with the given pattern.
#' If the pattern given ends with 'N', it looks for variables that also end with N.
#' If the pattern does not end with "N", it excludes variables that end with "N" while returning
#' those that start with the pattern. eg. pattern="BYVAR" may return c("BYVAR1","BYVAR2")
#' Whereas "BYVARN" may return c("BYVAR1N","BYVAR2N"), if they exist.
#'
#' @param df Dataframe input from which to get names
#' @param pattern String. If not ends with N,only matches start. If ends with N, matches start and
#' checks if column name ends in "N"
#'
#' @return Vector of column names starting with given pattern
#' @export
#'
#' @examples
#' data(ae_pre)
#' var_start(ae_pre$dsin, "BYVARN")
#' var_start(ae_pre$dsin, "BYVAR")
var_start <- function(df, pattern) {
  if (endsWith(pattern, "N")) {
    sort(names(df)[startsWith(names(df), sub("N$", "", pattern)) & (endsWith(names(df), "N"))])
  } else {
    sort(names(df)[startsWith(names(df), pattern) & (!endsWith(names(df), "N"))])
  }
}

#' Empty plot with message
#'
#' @param message Required message to be displayed within plot area
#' @param fontsize Set the font size of the message
#'
#' @return a list containing 2 objects
#' \itemize{
#' \item ptly - Interactive empty plot
#' \item plot - Static empty plot
#'  }
#' @export
#'
#' @examples
#' library(cvars)
#' empty_plot()
empty_plot <- function(message = "No data available for these values",
                       fontsize = 8) {
  g_plot <- ggplot() +
    annotate("text",
      x = 1, y = 1, size = fontsize,
      label = message
    ) +
    theme_void()
  fig <- plotly::ggplotly(g_plot, height = 200) %>%
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
    )
  return(list(plot = g_plot, ptly = fig))
}

#' Report Metadata
#'
#' @return `data.frame` containing report metadata
#' @noRd
#'
get_report_meta <- function() {
  tibble::tribble(
    ~TA,
    ~DOMAIN,
    ~REPNAME,
    ~REPDESC,
    ~REPNO,
    ~REPTYPE,
    "Any",
    "ADAE",
    "adae_r001",
    "Summary of Adverse Events by System Organ Class and Preferred Term",
    "2.1",
    "Table",
    "Any",
    "ADAE",
    "Forest Plot",
    "Forest plot for adverse events",
    "2.2",
    "Figure",
    "Any",
    "ADAE",
    "Volcano Plot",
    "Volcano plot for adverse events",
    "2.4",
    "Figure",
    "Any",
    "ADAE",
    "Event Analysis",
    "Event analysis of MedRA query",
    "2.5",
    "Figure",
  )
}

get_ae_term <- function() {
  aeTerm <- c(
    "Reported Term for the Adverse Event (AETERM)" = "AETERM",
    "AE Lowest Level Term (AELLT)" = "AELLT",
    "AE Dictionary-Derived Term (AEDECOD)" = "AEDECOD",
    "AE High Level Term (AEHLT)" = "AEHLT",
    "AE High Level Group Term (AEHLGT)" = "AEHLGT",
    "Primary System Organ Class (AESOC)" = "AESOC",
    "Body System or Organ Class (AEBODSYS)" = "AEBODSYS",
    "FMQ Name (FMQ_NAM)" = "FMQ_NAM"
  )
}
