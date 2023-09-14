#' Aligned Interval Plot for Adverse Events and Concomitant Medication
#'
#' @param datain Input list containing an element named `AE` (required) and an element named `CM`
#' with the corresponding dataframes
#' @param subjectid Participant ID (from the variable `USUBJID`)
#'
#' @return : a list containing 4 objects
#'  \itemize{
#'  \item ptly - Interactive plot output for display
#'  \item plot - Static plot output for saving externally
#'  \item rpt_data - Dataset used to create plot(s), for validation purpose
#'  \item title - Report title/header, for download purpose
#'  }
#' @export
#'
#' @examples
#' library(cvars)
#' data(adae)
#' data(cm)
#' multi_interval(
#'   datain = list(AE = adae, CM = cm),
#'   subjectid = "01-717-1446"
#' )$ptly
multi_interval <- function(datain,
                           subjectid) {
  # AE data is mandatory
  if (!"AE" %in% names(datain)) {
    return(empty_plot("No AE data available"))
  }
  # Get subject data for footer purposes
  aedata <- datain[["AE"]] %>% filter(USUBJID == subjectid)
  # Create AE plot - required
  seriesc <- c(MILD = "tan2", MODERATE = "dodgerblue3", SEVERE = "deeppink4")
  if ("ASEV" %in% names(aedata)) {
    seriesv <- "ASEV"
  } else if ("AESEV" %in% names(aedata)) {
    seriesv <- "AESEV"
  } else {
    seriesv <- NA
    seriesc <- "black"
  }
  ae_int <- interval_plot(
    datain = datain[["AE"]],
    startvar = "ASTDY",
    endvar = "AENDY",
    yvar = "AEDECOD",
    seriesvar = seriesv,
    subjectid = subjectid,
    series_color = seriesc,
    yaxislab = "Adverse Event"
  )
  # Plot title
  title <- paste0("Events over Study Period for Participant: ", subjectid, "\n")
  if ("AGE" %in% names(aedata)) title <- paste0(title, "; Age: ", unique(aedata$AGE))
  if ("SEX" %in% names(aedata)) title <- paste0(title, "; Sex: ", unique(aedata$SEX))
  if ("RACE" %in% names(aedata)) title <- paste0(title, "; Race: ", unique(aedata$RACE))
  # If Cm domain present, add plot
  if (("CM" %in% names(datain)) && (!is.null(datain$CM))) {
    cm_int <- interval_plot(
      datain = datain[["CM"]],
      startvar = "CMSTDY",
      endvar = "CMENDY",
      yvar = "CMTRT",
      seriesvar = "CMCLAS",
      subjectid = subjectid,
      series_color = NA,
      yaxislab = "Drug/Med/Therapy"
    )
    # Identify Axis range for combined plot
    if (nrow(cm_int$rpt_data) == 0 || nrow(ae_int$rpt_data) == 0) {
      ptly_out <- plotly::subplot(ae_int$ptly, cm_int$ptly,
        nrows = 2,
        titleX = TRUE, titleY = TRUE
      ) %>%
        plotly::layout(height = 600, showlegend = TRUE)
      plot_out <- cowplot::plot_grid(
        ae_int$plot, cm_int$plot,
        align = "v",
        nrow = 2
      )
    } else {
      rangeae <- layer_scales(ae_int$plot)$y$range$range
      rangecm <- layer_scales(cm_int$plot)$y$range$range
      xmin <- floor(min(rangeae, rangecm) / 5) * 5
      xmax <- ceiling(max(rangeae, rangecm) / 5) * 5
      sepby <- as.integer(seq(xmin, xmax, by = ceiling((xmax - xmin) / 10)))
      # Combine and align the 2 plots - interactive
      ptly_out <- plotly::subplot(ae_int$ptly, cm_int$ptly,
        shareX = TRUE, nrows = 2,
        titleX = TRUE, titleY = TRUE, margin = 0.005
      ) %>%
        plotly::layout(
          xaxis = list(
            range = c(xmin, xmax),
            tickmode = "array",
            tickvals = sepby,
            ticktext = sepby
          ),
          height = 600,
          legend = list(font = list(size = 9))
        )
      # Combine and align the 2 plots - static
      plot_out <- cowplot::plot_grid(
        ae_int$plot + scale_y_continuous(limits = c(xmin, xmax)),
        cm_int$plot + scale_y_continuous(limits = c(xmin, xmax)),
        align = "v",
        nrow = 2
      )
    }
    # Output data to save
    rptdata <- bind_rows(ae_int$rpt_data, cm_int$rpt_data)
    return(list(plot = plot_out, ptly = ptly_out, rpt_data = rptdata, title = title))
  } else {
    return(list(plot = ae_int$plot, ptly = ae_int$ptly, rpt_data = ae_int$rpt_data, title = title))
  }
}
