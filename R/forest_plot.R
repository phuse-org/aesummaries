#' Forest Plot of Adverse Events
#'
#' \code{forest_plot} takes arguments for displaying Risk and counts data (from \code{risk_stat})
#' in the form of a forest plot
#'
#' @param datain Input data from `risk_stat()` output, to be used for plotting
#' @param AE_Filter Adverse Events filter passed by user in `ae_pre_processor()`
#' @param review_by Event Variables (Higher and Lower Term) passed by user;
#' example: `c("AEBODSYS","AEDECOD")`
#' @param summary_by 'Patients' or 'Events' as passed to `risk_stat()`
#' to determine how counts were summed for risk calculation
#' @param statistics Risk statistic from user as passed to `risk_stat()`.
#' Possible values: `"Risk Difference"`, `"Risk Ratio"`
#' @param xref X intercept Value to draw vertical reference line in risk plot
#' @param pvalcut p value cutoff to determine and display significant differences
#' from control treatment
#' @param series_color Comma-separated string of colors for each treatment in
#' scatterplot or `NA` to use in-built colors
#' @param marker_shape Comma-separated string of point symbols for each
#' treatment in scatterplot. `NA` to use in-built
#' @param trtbign To display (N=count) next to treatment name in legend or not. Values: `"Y"`/`"N"`
#' @param scale_trans If log transformation needed for X axis of risk plot.
#' Possible values: `"identity"`/`"log10"`/`"log2"`/`"log"`
#'
#' @details
#' \itemize{
#' \item Arguments `AE_Filter`, `review_by`, `summary_by` and `statistics` are all used only
#' in axis
#' titles, tick labels or hover information
#' They should be the same values passed previously in the processing `ae_pre_processor()`
#' and analysis `risk_stat()` functions
#' \item Treatments with Risk statistic of a statistically significant p-value (below `pvalcut`)
#' are highlighted as 'Significantly Higher' or 'Significantly Lower' in the legend,
#' relative to the incidence rate of each term
#' on the control treatment.
#' \item Default colors for each treatment in scatter plot are preset in `series_color`.
#' Passing NA will change the palette to the package's default color scheme per `g_seriescol()`
#' To override manually, pass list of colors like this: "red,cyan,gold"
#' \item Shapes of symbols for each treatment in scatterplot are set to take the
#' package defaults per `g_seriessym()`.
#' To change them manually, pass list of shapes to `marker_shape` in format "1,6,25"
#' or "triangle,square,trianglefilled"
#' }
#'
#'
#' @return : a list containing 7 objects
#'  \itemize{
#'  \item ptly - Interactive plot output for display
#'  \item plot - Static plot output for saving externally
#'  \item drill_plt - Plot data used to create listing
#'  \item rpt_data - Dataset used to create forest plot, for validation purpose
#'  \item n - Number of adverse event terms in plot to determine plot height
#'  \item title - Report title/header
#'  \item footnote - Report footnote
#'  }
#' @export
#'
#' @examples
#' library(cvars)
#' data(ae_risk)
#' fp <- forest_plot(
#'   datain = ae_risk,
#'   AE_Filter = "Any",
#'   review_by = c("AEBODSYS", "AEDECOD"),
#'   summary_by = "Patients",
#'   statistics = "Risk Ratio"
#' )
#'
#' fp$ptly
#'
forest_plot <-
  function(datain,
           AE_Filter,
           review_by,
           summary_by,
           series_color = "black,royalblue2,goldenrod,orchid3,brown,pink,aquamarine1,tan4,skyblue1",
           marker_shape = NA,
           statistics,
           xref = 1,
           pvalcut = 0.05,
           trtbign = "Y",
           scale_trans = "identity") {
    ## If trtbign is Y (default) then treatment label gets (N=count) added:
    if (trtbign == "Y") {
      datain <- datain %>% mutate(TRTTXT = paste0(TRTVAR, " (N=", TOTAL_N, ")"))
    } else {
      datain <- datain %>% mutate(TRTTXT = TRTVAR)
    }

    # Add High Term to hover_text if not already existing:
    if (!any((str_detect(datain$HOVER_TEXT, review_by[1])))) {
      if ("BYVAR1" %in% names(datain)) {
        datain <- datain %>%
          mutate(HOVER_TEXT = paste0("\n", review_by[1], " = ", BYVAR1, HOVER_TEXT))
      }
    }

    # Variable carried forward as treatment is TRTTXT.

    # For height of plot calculation, depending on number of terms:
    nterm <- length(unique(datain$DPTVAL))
    adjh <- max(nterm * 20, 600)

    ### Percentage Scatter Plot data preparation:
    # Change hover information to exclude Risk and include Treatment
    dat_out <-
      datain %>%
      filter(RISK != Inf) %>%
      group_by(DPTVAL, TRTVAR) %>%
      mutate(HOVER_PCT = paste0(
        gsub(
          "Risk.*$", "",
          HOVER_TEXT
        ), PCT, "%\n",
        TRTVAR
      )) %>%
      arrange(desc(row_number()))

    # Convert Terms to factor to retain sorting order of getstats
    dat_out$DPTVAL <- fct_inorder(dat_out$DPTVAL)

    # Dynamic Plot width - review to update later
    if (max(nchar(unique(dat_out$TRTPAIR))) > 60) {
      plotwid <- 2000
    } else if (max(nchar(unique(dat_out$TRTPAIR))) < 60 ||
      max(nchar(unique(dat_out$TRTPAIR))) > 40) {
      plotwid <- 1500
    } else {
      plotwid <- 1200
    }
    ## Color/shape aesthetics for treatment:

    ctrl <- trimws(strsplit(unique(dat_out$TRTPAIR), " -vs- ")[[1]][1])
    # Set order such that control group will always be first
    dat_out$TRTTXT <-
      factor(dat_out$TRTTXT,
        levels = unique(dat_out$TRTTXT[order(dat_out$TRTVAR != ctrl, dat_out$TRTTXT)])
      )
    # colors for Scatter Points:
    trtcols <- g_seriescol(dat_out, series_color, "TRTTXT")
    # Shapes for Scatter Points:
    trtshapes <- g_seriessym(dat_out, marker_shape, "TRTTXT")

    ## Key for listings:
    dat_out$key <- row.names(dat_out)


    ## Identifying significant points higher/lower than control:
    hltpts <- dat_out %>%
      filter(PVALUE < pvalcut) %>%
      mutate(effect = ifelse(PCT > CTRL_PCT, "Significantly Higher",
        ifelse(PCT < CTRL_PCT, "Significantly Lower", "")
      )) %>%
      filter(effect != "")
    # Color green or red for + and - effects
    hltfill <- setNames(c("red", "green"), c("Significantly Higher", "Significantly Lower"))

    ## To Draw line between each term in all plots:
    liney <- seq(1.5, length(unique(dat_out$DPTVAL)) - 0.5, 1)

    ### ScatterPlot creation
    sp <- ggplot(
      data = dat_out,
      aes(
        x = PCT,
        y = DPTVAL,
        color = TRTTXT,
        shape = TRTTXT,
        text = HOVER_PCT,
        key = key
      )
    ) +
      geom_point(size = 0.9) +
      scale_color_manual(
        name = "",
        values = trtcols
      ) +
      scale_shape_manual(name = "", values = trtshapes) +
      theme_bw() +
      scale_y_discrete() +
      geom_hline(
        yintercept = liney, linetype = "dotted",
        color = "black", size = 0.2, alpha = 0.5
      ) +
      scale_x_continuous(position = "top") +
      geom_point(
        data = hltpts,
        aes(
          x = PCT, y = DPTVAL, fill = effect,
          text = HOVER_PCT, key = key
        ),
        inherit.aes = FALSE, shape = 23, size = 2, stroke = 0.2
      ) +
      scale_fill_manual(name = "", values = hltfill) +
      xlab("Percentage") +
      theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )

    # for static:
    sp1 <- sp + theme(legend.position = "bottom", legend.direction = "horizontal")

    # For interactive
    splotly <- plotly::ggplotly(
      sp,
      tooltip = c("text"),
      height = adjh,
      source = "plot_output"
    )

    # Fixing legend with parantheses
    for (i in seq_along(splotly$x$data)) {
      if (!is.null(splotly$x$data[[i]]$name)) {
        splotly$x$data[[i]]$name <- sub(
          "\\(", "",
          sapply(lapply(strsplit(
            splotly$x$data[[i]]$name,
            ","
          ), head, -1), paste, collapse = ",")
        ) # [1]
      }
    }
    splotly <- splotly %>%
      plotly::layout(xaxis = list(side = "top"))


    ### Prepare Data for forest plot:
    # Adding treatment pair, remove count from hover info
    dat_out <- dat_out %>%
      mutate(HOVER_PAIR = paste0(sub("n of.*Risk", "Risk", HOVER_TEXT), " \n", TRTPAIR))

    # X Limits - max risk rounded up to the nearest 0.5
    xlims <- c(0, ceiling(max(dat_out$RISK) / 0.5) * 0.5)

    # Line/forest plot of risk statistic:

    fp <- ggplot(
      dat_out,
      aes(
        y = DPTVAL,
        x = RISK,
        xmin = RISKCIL,
        xmax = RISKCIU,
        text = HOVER_PAIR,
        group = TRTPAIR, color = TRTPAIR
      )
    ) +
      ggstance::geom_errorbarh(
        height = 0.1,
        position = ggstance::position_dodgev(height = 0.6), size = 0.5
      ) +
      geom_point(
        shape = 22,
        size = 0.6,
        position = ggstance::position_dodgev(height = 0.6)
      ) +
      geom_vline(xintercept = xref, linetype = 3) +
      geom_hline(
        yintercept = liney, linetype = "dotted",
        color = "black", size = 0.2, alpha = 0.5
      ) +
      labs(x = statistics, color = NULL) +
      scale_y_discrete() +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 7),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )

    if (str_detect(scale_trans, "log")) {
      fp <- fp + scale_x_continuous(position = "top", trans = scale_trans)
    } else {
      fp <- fp + scale_x_continuous(position = "top") +
        coord_cartesian(xlim = xlims)
    }
    # Interactive:
    fplotly <- plotly::ggplotly(fp,
      tooltip = c("text"),
      height = adjh,
      source = "plot_output"
    ) %>%
      plotly::layout(xaxis = list(side = "top"))

    # for static download:
    fp1 <- fp + theme(legend.position = "bottom", legend.direction = "horizontal")

    ### Base for tabular data - currently only p value
    base <- ggplot(dat_out, aes(y = DPTVAL)) +
      theme(
        plot.title = element_text(hjust = 0.1, size = 10),
        axis.text.x = element_text(
          color = "white",
          hjust = -3,
          size = 25
        ),
        # alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )


    ## Table to display Terms:

    # To get X axis label for terms, use review_by parameter:
    if (length(review_by) > 1) term_lab <- review_by[2] else term_lab <- review_by
    term_lab <- recode(term_lab,
      "AETERM" = "Reported Term for the Adverse Event",
      "AELLT" = "AE Lowest Level Term",
      "AEDECOD" = "AE Dictionary-Derived Term",
      "AEHLT" = "AE High Level Term",
      "AEHLGT" = "AE High Level Group Term",
      "AESOC" = "Primary System Organ Class",
      "AEBODSYS" = "Body System or Organ Class",
      "SMQ_NAM" = "SMQ Name",
      "FMQ_NAM" = "FMQ Name",
      "CQ_NAM" = "Customized Query Name"
    )

    if (all(dat_out$DPTVAL == toupper(dat_out$DPTVAL))) {
      fsize <- 2.2
    } else {
      fsize <- 2.4
    }
    termtable <- base +
      xlab(term_lab) +
      geom_text(aes(y = DPTVAL, label = DPTVAL, text = DPTVAL),
        size = fsize,
        x = 0
      ) +
      scale_x_continuous(position = "top")

    # Terms interactive:
    termplotly <-
      plotly::ggplotly(
        termtable,
        tooltip = c("text"),
        height = adjh,
        source = "plot_output"
      ) %>%
      plotly::layout(xaxis = list(side = "top")) %>%
      plotly::style(textposition = "right")

    # Static output:
    termstat <- base +
      xlab(term_lab) +
      geom_text(
        aes(y = DPTVAL, label = DPTVAL, text = DPTVAL),
        x = 0.2,
        size = fsize,
        hjust = 0
      ) +
      scale_x_continuous(position = "top")

    ### p-value Table graph
    ptable <-
      base + scale_x_discrete(position = "top") +
      geom_text(
        aes(x = TRTPAIR, y = DPTVAL, label = PVALUE, color = TRTPAIR),
        size = 2.4,
        hjust = 0
      ) + xlab("p-value") +
      theme(legend.position = "none")

    # Plotly P value table
    tplotly <- plotly::ggplotly(ptable,
      height = adjh,
      source = "plot_output", showlegend = FALSE
    ) %>%
      plotly::layout(xaxis = list(side = "top")) %>%
      plotly::style(hoverinfo = "none", textposition = "middle center", showlegend = FALSE)


    ### Combine interactive plots into subplots for display
    inter_fig <- plotly::subplot(
      termplotly,
      splotly,
      fplotly,
      tplotly,
      nrows = 1,
      widths = c(0.22, 0.32, 0.33, 0.13),
      titleX = TRUE,
      margin = 0.003
    )

    inter_fig <- inter_fig %>%
      plotly::layout(
        margin = list(l = 0, b = 1, r = 0, t = 0), width = plotwid,
        showlegend = TRUE,
        legend = list(
          orientation = "h", x = 0.5, y = -0.1,
          size = 8, xanchor = "center", yanchor = "top", font = list(size = 8)
        )
      )



    inter_fig$x$source <- "plot_output"

    ### Combine for For static ggplot output for download purposes:
    row1 <- cowplot::plot_grid(
      termstat,
      sp + theme(legend.position = "none"),
      fp + theme(legend.position = "none"),
      ptable,
      align = "h",
      nrow = 1,
      rel_widths = c(2, 3.5, 3.5, 1)
    )
    # Separate legend from scatterplot:
    leg1 <- cowplot::get_legend(sp1)
    leg2 <- cowplot::get_legend(fp1)
    leg <- cowplot::plot_grid(leg1, leg2, nrow = 1, rel_widths = c(0.4, 0.6))
    # Combine with legend:
    static_fig <- cowplot::plot_grid(
      row1,
      leg,
      nrow = 2,
      ncol = 1,
      rel_heights = c(0.9, 0.1)
    )

    # Data to be returned:
    out <- dat_out %>%
      ungroup() %>%
      select(
        starts_with("BYVAR"),
        DPTVAL, TRTTXT, TRTVAR,
        PCT,
        HOVER_PCT,
        RISK,
        RISKCIL,
        RISKCIU,
        PVALUE
      )


    # Title & Foot Notes
    if (is.null(AE_Filter)) AE_Filter <- ""
    AE_cond <- ifelse(
      length(AE_Filter) <= 1,
      AE_Filter,
      paste0(
        paste0(
          AE_Filter[-length(AE_Filter)],
          collapse = ", "
        ),
        " and ",
        AE_Filter[length(AE_Filter)]
      )
    )
    title <- paste0("Forest plot for ", statistics, " of ", AE_cond, " Adverse Events")
    footnote <- paste0(
      "* N is the total number of ",
      ifelse(tolower(summary_by) == "patients", "participants", "events"),
      ". \nClassifications of adverse events are based on the Medical Dictionary for Regulatory ",
      "Activities (MedDRA v21.1). \nDashed Vertical line represents risk value reference line \n",
      "Totals for the No. of Participants/Events at a higher level are not necessarily ",
      "the sum of those at the lower levels since a participant may report two or more \n",
      ifelse(tolower(summary_by) == "patients",
        "The number of participants reporting at least 1 occurrence of the event specified.",
        "Event counts are the sum of individual occurrences within that category"
      )
    )

    return(list(
      ptly = inter_fig,
      plot = static_fig,
      drill_plt = sp,
      rpt_data = out,
      n = nterm,
      title = title,
      footnote = footnote
    ))
  }
