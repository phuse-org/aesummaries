#' Patient-Level Interval Plot
#'
#' @param datain Input events/concomitant medication dataset
#' @param startvar Event/Treatment Start Day Variable name (X axis)
#' @param endvar Event/Treatment End Day Variable name (X axis)
#' @param yvar Variable containing event terms or medications to be plotted in Y axis
#' @param seriesvar Variable to stratify plot by color or NA to have no stratification
#' @param subjectid Selected `USUBJID` variable value
#' @param series_color List of colors to be assigned to levels in `seriesvar` or
#' NA to use package defaults
#' @param xaxislab Label for plot X axis
#' @param yaxislab Label for plot Y axis
#'
#' @return : a list containing 3 objects
#'  \itemize{
#'  \item ptly - Interactive plot output for display
#'  \item plot - Static plot output for saving externally
#'  \item rpt_data - Dataset used to create forest plot, for validation purpose
#'  }
#' @export
#'
#' @examples
#' data(adae)
#' interval_plot(
#'   datain = adae,
#'   startvar = "ASTDY",
#'   endvar = "AENDY",
#'   yvar = "AEDECOD",
#'   seriesvar = "AESEV",
#'   subjectid = "01-701-1302",
#'   series_color = NA,
#'   yaxislab = "Reported Term for the Adverse Event"
#' )$ptly
interval_plot <- function(datain,
                          startvar,
                          endvar,
                          yvar,
                          seriesvar,
                          subjectid,
                          series_color = NA,
                          xaxislab = "Start and End Study Day",
                          yaxislab = "") {
  # Filter subject and get info
  ad_plot <- datain %>%
    filter(USUBJID == subjectid) %>%
    mutate(Status = case_when(
      (is.na(get(startvar)) & get(endvar) >= 0) ~ "End Day",
      (is.na(get(endvar)) & get(startvar) >= 0) ~ "Start Day",
      (!is.na(get(startvar)) & !is.na(get(endvar))) ~ "Complete",
      TRUE ~ "Remove"
    )) %>%
    filter(Status != "Remove")
  # Check if no data left and return accordingly
  if (nrow(ad_plot) == 0) {
    output <- empty_plot(
      message = "No data for this participant/period",
      fontsize = 4
    )
    return(list(ptly = output$ptly, plot = output$plot, rpt_data = ad_plot))
  }
  if (is.na(seriesvar) || (!seriesvar %in% names(ad_plot))) {
    seriesvar <- "TOTAL"
    ad_plot[[seriesvar]] <- "All"
  }
  # If colors not preset - set here for analysis variable
  if (all(is.null(names(series_color)))) {
    series_color <- g_seriescol(ad_plot, series_color, seriesvar)
  }
  # Hover Information:
  ad_plot <- ad_plot %>%
    select(all_of(c("USUBJID", startvar, endvar, yvar, seriesvar, "Status"))) %>%
    mutate(
      HOVER_TEXT = paste0(
        !!sym(yvar), "\n",
        "Start Day: ", !!sym(startvar), "\n",
        "End Day: ", !!sym(endvar), "\n",
        !!sym(seriesvar)
      ),
      HOVER_TEXT = gsub(": NA", ": -", HOVER_TEXT),
      !!yvar := as.factor(!!sym(yvar))
    )
  # Use data with both dates present for segment plot
  segmentdata <- ad_plot %>%
    filter(Status == "Complete")

  # Use data with either Start or end dates only for scatter plot
  scatterdata <- ad_plot %>%
    filter(Status != "Complete" | !!sym(startvar) == !!sym(endvar)) %>%
    tidyr::pivot_longer(c(startvar, endvar), names_to = "key", values_to = "Value") %>%
    filter(!is.na(Value)) %>%
    select(-key) %>%
    distinct(.keep_all = TRUE)

  # Create ggplot object - segment plot for Complete intervals and scatter for incomplete
  gplot <- ggplot(mapping = aes(text = HOVER_TEXT)) +
    scale_color_manual(name = "", values = series_color) +
    labs(x = yaxislab, y = xaxislab) +
    coord_flip() +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 10),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  # Check if data exists for intervals
  if (nrow(segmentdata) != 0) {
    gplot <- gplot +
      geom_linerange(
        data = segmentdata,
        aes(
          ymin = !!sym(startvar), ymax = !!sym(endvar),
          x = !!sym(yvar), group = !!sym(seriesvar),
          color = !!sym(seriesvar)
        ),
        position = position_dodge(width = 0.3),
        size = 1.2
      )
    # Convert to plotly object
    splotly <- plotly::ggplotly(gplot, tooltip = "text")
  }

  # Check if data exists for dot plot
  if (nrow(scatterdata) != 0) {
    # Assign shapes by Status
    shapelist <- c("End Day" = 18, "Start Day" = 17, "Complete" = 15)
    gplot <- gplot +
      geom_point(
        data = scatterdata,
        aes(
          y = Value, x = !!sym(yvar),
          shape = Status,
          color = !!sym(seriesvar)
        ),
        size = 1.5,
        position = position_dodge(width = 0.3)
      ) +
      scale_shape_manual(name = "", values = shapelist)
    # Convert to plotly object
    splotly <- plotly::ggplotly(gplot, tooltip = "text")
  }

  # To remove the duplicate values in legend and edit labels
  legdf <- data.frame(
    id = seq_along(splotly$x$data),
    legend_entries = unlist(lapply(splotly$x$data, "[[", "name"))
  )
  legdf$legend_group <- gsub(
    "\\(|\\)", "",
    gsub(",1", ",Complete", legdf$legend_entries)
  )
  legdf$is_first <- !duplicated(legdf$legend_group)
  for (i in legdf$id) {
    splotly$x$data[[i]]$name <- legdf$legend_group[[i]]
    splotly$x$data[[i]]$legendgroup <- splotly$x$data[[i]]$name
    if (!legdf$is_first[[i]]) splotly$x$data[[i]]$showlegend <- FALSE
  }
  return(list(plot = gplot, ptly = splotly, rpt_data = ad_plot))
}
