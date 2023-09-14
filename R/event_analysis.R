#' @title To generate event graphs for FDA Medical queries
#'
#' @param datain input dataset
#' @param datain_N input adsl bign dataset with tretament counts based on the population
#' @param hl_var MedDRA queries or high level variables valid value : "FMQ_NAM","AEBODSYS"
#' @param hl_val Queries/term name captured in the `hl_var` variable, One selection e.g.,
#' erythema
#' @param hl_scope scope of the MedDRA queries; valid value : Narrow or Broad
#' @param ll_var MedDRA variable; valid value : AEDECOD
#' @param ll_val MedDRA value, One selection e.g., erythema
#' @param ll_scope scope of the MedDRA queries; valid value : Narrow or Broad
#' @param summary_by  set to 'Events' or 'patients' based on the requirement to ass the titles and
#' footnotes
#' @param ref_line y axis position of the Horzondal reference line
#'
#' @return generated the normal plot and interactive plotly
#' @export
#'
#' @examples
#' library(cvars)
#' data("event_df")
#' goutput <- event_analysis(
#'   datain = event_df$dsin,
#'   datain_N = event_df$dout,
#'   hl_var = "FMQ_NAM",
#'   hl_val = "ABDOMINAL PAIN",
#'   hl_scope = "Narrow",
#'   ll_var = "AEDECOD",
#'   ll_val = "ABDOMINAL DISCOMFORT",
#'   ll_scope = "Narrow",
#'   summary_by = "Events",
#'   ref_line = 2
#' )
#' goutput$plot
#'
event_analysis <- function(datain = NULL,
                           datain_N = NULL,
                           hl_var = "FMQ_NAM",
                           hl_val = "erythema",
                           hl_scope = "Narrow",
                           ll_var = "AEDECOD",
                           ll_val = "erythema",
                           ll_scope = "Narrow",
                           summary_by = "Events",
                           ref_line = 2) {
  names(datain) <- toupper(names(datain))

  # Calculating subject count for higher level event variable
  event_mcat <- mcatstat(
    datain = datain,
    d_datain = datain_N,
    ui_uniqid = ifelse(summary_by == "Patients", "USUBJID", NA),
    ui_dptvar = ll_var,
    ui_pctdisp = "TRT"
  )

  # concatenating higher level event variable and scope
  if (hl_var %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
    if (toupper(hl_scope) == "NARROW") {
      hl_val1 <- paste0(str_to_upper(hl_val), "/", str_to_upper(hl_scope))
    } else {
      hl_val1 <- str_to_upper(hl_val)
    }
  } else {
    hl_val1 <- hl_val
  }
  # concatenating lower level event variable and scope
  if (ll_var %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
    if (toupper(ll_scope) == "NARROW") {
      ll_val1 <- paste0(str_to_upper(ll_val), "/", str_to_upper(ll_scope))
    } else {
      ll_val1 <- str_to_upper(hl_val)
    }
  } else {
    ll_val1 <- ll_val
  }

  # Sub setting the higher level event variable
  if (hl_var %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
    hl_summ <- event_mcat %>% # filter(BYVAR1%in%hl_val1) %>%
      filter(str_detect(toupper(BYVAR1), toupper(hl_val1))) %>%
      mutate(Percent = paste(PCT, "% \n Low Term:", DPTVAL)) %>%
      group_by(TRTVAR) %>%
      mutate(
        pct = as.numeric(PCT),
        DECODh = ifelse(str_detect(toupper(DPTVAL), toupper(ll_val1)), 9999, rank(PCT))
      )
  } else {
    hl_summ <- event_mcat %>% # filter(BYVAR1%in%hl_val1) %>%
      filter(toupper(BYVAR1) == toupper(hl_val1)) %>%
      mutate(Percent = paste(PCT, "% \n Low Term:", DPTVAL)) %>%
      group_by(TRTVAR) %>%
      mutate(
        pct = as.numeric(PCT),
        DECODh = ifelse(str_detect(toupper(DPTVAL), toupper(ll_val1)), 9999, rank(PCT))
      )
  }
  # Sub setting the lower level event variable
  if (ll_var %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
    ll_summ <- event_mcat %>%
      filter(str_detect(toupper(DPTVAL), toupper(ll_val1))) %>%
      mutate(pct = as.numeric(PCT), Percent = paste(pct, "%")) %>%
      arrange(TRTVAR, pct)
  } else {
    ll_summ <- event_mcat %>%
      filter(toupper(DPTVAL) == toupper(ll_val1)) %>%
      mutate(pct = as.numeric(PCT), Percent = paste(pct, "%")) %>%
      arrange(TRTVAR, pct)
  }
  # Getting Max PCT value for stacked plot to have same scale for both plots
  yaxis_max <- hl_summ %>%
    group_by(TRTVAR) %>%
    summarise(pct_s = sum(pct))
  yscal_max <-
    ifelse(max(yaxis_max$pct_s) < 5,
      max(yaxis_max$pct_s) + 10,
      max(yaxis_max$pct_s) + 5
    )
  yscal_br <- ifelse(yscal_max > 40, 5, 2)

  # Generating bar chart for PT level
  pt_plot <- ll_summ %>%
    ggplot(aes(x = TRTVAR, y = pct, text = Percent)) +
    geom_bar(
      stat = "identity",
      width = 0.4,
      fill = "royalblue3",
      color = "black",
      size = 0.4
    ) +
    scale_y_continuous(
      limits = c(0, yscal_max),
      breaks = seq(0, yscal_max, yscal_br)
    ) +
    labs(x = "Treatment", y = "Percentage of Participants", colour = NULL) +
    geom_hline(yintercept = ref_line, linetype = "dashed") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 15,
        hjust = 1,
        size = 8
      ),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(size = 8),
      legend.text = element_text(size = 5),
      plot.margin = unit(c(1, 0, 0, 1.5), "cm")
    )

  pt_plot1 <- pt_plot + ggtitle(paste0(str_to_title(ll_val), " PT"))
  pt_ptly <- plotly::ggplotly(pt_plot, tooltip = "text", source = "plot_output") %>%
    plotly::add_annotations(
      text = paste0(str_to_title(ll_val), " PT"),
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      yshift = 35,
      font = list(size = 12)
    )

  # Generating bar chart for query level
  all_cols <- grDevices::colors()[grep("gr(a|e)|royalblue", grDevices::colors(), invert = TRUE)]
  manualcolors <- c(
    "royalblue3",
    all_cols[seq(length(all_cols), 3,
      length.out = length(unique(hl_summ$DPTVAL))
    )]
  )
  query_plot <- hl_summ %>%
    ggplot(aes(
      x = TRTVAR,
      y = pct,
      fill = reorder(DPTVAL, -DECODh),
      group = DECODh,
      text = Percent
    )) +
    geom_bar(
      position = "stack",
      stat = "identity",
      width = 0.5,
      color = "black",
      size = 0.4
    ) +
    scale_y_continuous(
      limits = c(0, yscal_max),
      breaks = seq(0, yscal_max, yscal_br)
    ) +
    scale_fill_manual(values = manualcolors) +
    labs(x = "Treatment", y = "Percentage of Participants", colour = NULL) +
    geom_hline(yintercept = ref_line, linetype = "dashed") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 15,
        hjust = 1,
        size = 8
      ),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(size = 8),
      legend.text = element_text(size = 5),
      plot.margin = unit(c(1, 0, 0, 1.5), "cm")
    )
  query_plot1 <-
    query_plot + ggtitle(str_wrap(paste0(
      toupper(sub("\\_.*", "", hl_var)),
      " Categorization of ", str_to_title(hl_val)
    ), width = 80))
  query_ptly <- plotly::ggplotly(query_plot, tooltip = "text", source = "plot_output") %>%
    plotly::add_annotations(
      text = str_wrap(paste0(
        toupper(sub("\\_.*", "", hl_var)),
        " Categorization of ", str_to_title(hl_val)
      ), width = 80),
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      yshift = 35,
      font = list(size = 12)
    ) %>%
    plotly::layout(legend = list(title = ""))
  inter_fig <- plotly::subplot(
    pt_ptly,
    query_ptly,
    titleY = TRUE,
    shareX = TRUE,
    shareY = TRUE,
    widths = c(0.5, 0.5),
    margin = 0.005
  ) %>% plotly::layout(showlegend = TRUE, height = 600)

  static_fig <- cowplot::plot_grid(
    pt_plot1,
    query_plot1,
    nrow = 1,
    align = "h",
    rel_widths = c(4, 6)
  )


  title <- paste0("Event Analysis plot of Adverse Events")
  footnote <- paste0(
    "* N is the total number of ",
    ifelse(tolower(summary_by) == "patients", "participants", "events"),
    ". \n",
    "Classifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). \n", # nolint
    "FMQ classification is based on FDA FMQ consolidated list. \n",
    "Dashed Horizontal line represents incidence percentage reference line. \n",
    "Totals for the No. of Participants/Events at a higher level are not necessarily the sum of those at the lower levels since a participant may report two or more. \n", # nolint
    "PT - Preferred Term ; FMQ - FDA MedDRA Queries \n",
    ifelse(
      tolower(summary_by) == "patients",
      "The number of participants reporting at least 1 occurrence of the event specified.",
      "Event counts are the sum of individual occurrences within that category."
    )
  )

  # return list of plots
  return(
    list(
      ptly = inter_fig,
      plot = static_fig,
      rpt_data = hl_summ,
      rpt_data1 = ll_summ,
      title = title,
      footnote = footnote
    )
  )
}
