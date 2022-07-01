options(dplyr.summarise.inform = F)
Forest_Plot <-
  function(dat,
           groupvar,
           review_by,
           summary_by,
           statistics,
           xlims = c(0, 3),
           xref = 1) {
    groupvar = "TRTVAR"
    if (review_by == "PT") {
      byterm = "AEDECOD"
    } else if (review_by == "SOC") {
      byterm = "AEBODSYS"
    }
    nterm = length(unique(dat[[byterm]]))
    adjh <- max(nterm * 15, 600) #length of plot
    #Calculate percentage:
    dat_out <-
      dat %>% filter(TEST != Inf) %>% group_by(across(all_of(c(byterm, groupvar)))) %>%
      mutate(
        pct = ifelse(N1 == 0, PCT2, PCT1),
        Percentage = paste0(pct, "%"),
        SOC = AEBODSYS,
        PT = ifelse(review_by == "PT", AEDECOD, "")
      ) %>% arrange(desc(row_number()))
    dat_out[[byterm]] <- fct_inorder(dat_out[[byterm]])
    #adjw <- ifelse(review_by=="PT",700,900)
    #Scatterplot of percentage vs preferred term
    key <- row.names(dat_out)
    sp = ggplot(
      dat_out,
      aes_string(
        x = "pct",
        y = byterm,
        color = groupvar,
        shape = groupvar,
        text = "Percentage",
        key = "key",
        soc = "SOC",
        pt = "PT"
      )
    ) +
      geom_point(size = 1, position = position_dodge(width = NULL)) +
      scale_color_manual(name = "", values = c("red", "blue")) +
      scale_shape_manual(name = "", values = c(17, 16)) + theme_bw() +
      #scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
      scale_y_discrete() +
      scale_x_continuous(position = "top") +
      xlab("Percentage") +
      theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )
    #for plotly purposes
    sp1 = sp + theme(legend.position = "bottom", legend.direction = "horizontal")
    if (review_by == "PT") {
      splotly = ggplotly(
        sp,
        tooltip = c("text", "soc", "pt"),
        height = adjh,
        source = "plot_output"
      )
    } else{
      splotly = ggplotly(
        sp,
        tooltip = c("text", "soc"),
        height = adjh,
        source = "plot_output"
      )
    }
    splotly <- splotly %>%
      layout(xaxis = list(side = "top"))#,tickfont=list(size=10)))
    
    #Data for forest plot:
    #Risk values
    dat_out <- dat_out %>%
      mutate(Risk_CI = paste0(
        round(TEST, 2),
        " (",
        round(TESTCIL, 2),
        ",",
        round(TESTCIU, 2),
        ")"
      ))
    #Line/forest plot of risk ratio:
    fp = ggplot(
      dat_out,
      aes_string(
        y = byterm,
        x = "TEST",
        xmin = "TESTCIL",
        xmax = "TESTCIU",
        text = "Risk_CI"
      )
    ) +
      geom_errorbarh(height = 0.1, color = "black") +
      geom_point(shape = 22,
                 size = 1,
                 fill = "black") +
      geom_vline(xintercept = xref, linetype = 3) +
      xlab(statistics) +
      scale_y_discrete() +
      coord_cartesian(xlim = xlims) +
      theme_bw() +
      scale_x_continuous(position = "top") +
      theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )
    
    fplotly = ggplotly(fp,
                       tooltip = c("text"),
                       height = adjh,
                       source = "plot_output") %>%
      layout(xaxis = list(side = "top"))#,tickfont=list(size=10)))
    
    ###Base for tabular data - currently only p value
    base <- ggplot(dat_out, aes_string(y = byterm)) +
      theme(
        plot.title = element_text(hjust = 0.1, size = 10),
        axis.text.x = element_text(
          color = "white",
          hjust = -3,
          size = 25
        ),
        #alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )
    
    ##Table to display SOC or PT:
    
    fsize <- ifelse(review_by == "PT", 2.4, 2.2)
    termtable <- base +
      xlab(review_by) +
      geom_text(aes_string(y = byterm, label = byterm, text = byterm),
                size = fsize,
                x = 0) +
      scale_x_continuous(position = "top")
    
    #PT/SOC interactive:
    termplotly <-
      ggplotly(
        termtable,
        tooltip = c("text"),
        height = adjh,
        source = "plot_output"
      ) %>%
      layout(xaxis = list(side = "top")) %>%
      style(textposition = "right")
    
    #Static output:
    termstat <- base +
      xlab(review_by) +
      geom_text(
        aes_string(y = byterm, label = byterm, text = byterm),
        x = 0.2,
        size = fsize,
        hjust = 0
      ) +
      scale_x_continuous(position = "top")#,limits = c(0,2.5))
    
    ##Display P values
    ptable <-
      base + xlab("p-value") + scale_x_continuous(position = "top") +
      geom_text(
        aes_string(y = byterm, label = "pvalue"),
        size = 2.4,
        x = 0.3,
        hjust = 0
      )
    
    #Plotly P value table
    tplotly = ggplotly(ptable, height = adjh, source = "plot_output") %>% layout(xaxis =
                                                                                   list(side = "top")) %>%
      style(hoverinfo = "none", textposition = "right")
    
    
    ###Combine interactive plots into subplots for display
    inter_fig = subplot(
      termplotly,
      splotly,
      fplotly,
      tplotly,
      widths = c(0.2, 0.35, 0.35, 0.1),
      titleX = TRUE,
      margin = 0.004
    ) %>%
      layout(showlegend = T,
             legend = list(
               orientation = "h",
               x = 0.2,
               y = 0,
               size = 15
             ))#,
    
    inter_fig$x$source <- "plot_output"
    ###Combine for For static ggplot output for download purposes:
    #relw = ifelse(review_by=="PT",c(2,3.5,3.5,1),c(2.5,3.5,3,1))
    row1 = plot_grid(
      termstat,
      sp + theme(legend.position = "none"),
      fp,
      ptable,
      align = "h"
      ,
      nrow = 1,
      rel_widths = c(2, 3.5, 3.5, 1)
    )
    #Separate legend from scatterplot:
    leg = get_legend(sp1)
    #Combine with legend:
    static_fig = plot_grid(
      row1,
      leg,
      nrow = 2,
      ncol = 1,
      rel_heights = c(0.9, 0.1)
    )
    
    #Data to be returned:
    out <- dat_out %>%
      select(all_of(c(byterm, groupvar)),
             pct,
             Percentage,
             TEST,
             TESTCIL,
             TESTCIU,
             Risk_CI,
             pvalue,
             SOC)
    if (review_by == "PT") {
      out = out %>% rename(AEBODSYS = SOC)
    } else if (review_by == "SOC") {
      out = out %>% select(-SOC)
    }
    
    return(list(
      ptly = inter_fig,
      plot = static_fig,
      drill_plt = sp,
      data = out,
      n = nterm
    ))
  }
