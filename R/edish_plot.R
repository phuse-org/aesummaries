################################################################################
# volcano_plot.R
# 1 Function: Generation of volcano plot along with specifications.
################################################################################
edish <- function(datain,
                  subset,
                  xaxisopt,
                  yaxisopt,
                  xrefline,
                  yrefline) {
  names(datain) <- toupper(names(datain))
  
  hy_data = datain %>% filter(eval(parse(text = subset)) &
                                !TRTVAR %in% c("N/A", "null", ""))
  
  hy <- hy_data %>% mutate(maxv = AVAL / ANRHI) %>%
    mutate(PARM = ifelse(
      grepl("alanineaminotransferase", tolower(gsub("\\s", "", PARAM))),
      "alt",
      ifelse(
        grepl("aspartateaminotransferase", tolower(gsub("\\s", "", PARAM))),
        "ast",
        ifelse(grepl("bilirubin", tolower(
          gsub("\\s", "", PARAM)
        )), "bili", "")
      )
    )) %>%
    group_by(USUBJID, TRTVAR, PARAMCD, PARM) %>%
    summarise(x = max(maxv)) %>%
    pivot_wider(
      id_cols = c(USUBJID, TRTVAR),
      names_from = PARM,
      values_from = x
    ) %>%
    mutate(
      astalt = pmax(ast, alt),
      text = paste0(
        "Subjid = ",
        USUBJID,
        "\n",
        "Max of ALT/AST = ",
        astalt,
        "\n",
        "Bilirubin = ",
        bili
      )
    ) %>%
    select(USUBJID, TRTVAR, astalt, bili, text)%>%na.omit()
  
  key <- row.names(hy)
  
  quad_lables = list(
    text1 = c(
      "Potential Hy's Law Cases",
      "Temple's Corollary",
      "Gilberts Syndrome or Cholestasis",
      "Normal"
    ),
    x = c(ceiling(max(hy$astalt)) - 1, ceiling(max(hy$astalt)) -
            1, 1, 0.2),
    y = c(ceiling(max(hy$bili)), 0, ceiling(max(hy$bili)), 0)
  )
  
  #### identifying plot options ####
  if (!is.null(xaxisopt)) {
    xlimits <- as.numeric(unlist(strsplit(xaxisopt[2], ",")))
    if (xlimits[2] > ceiling(max(hy$astalt)))
      xlimits[2] <- ceiling(max(hy$astalt))
    xbreaks <- as.numeric(unlist(strsplit(xaxisopt[1], ",")))
    if (max(xbreaks) > ceiling(max(hy$astalt)))
      xbreaks[which.max(xbreaks)] <- ceiling(max(hy$astalt))
  }
  
  if (!is.null(yaxisopt)) {
    ylimits <- as.numeric(unlist(strsplit(yaxisopt[2], ",")))
    if (ylimits[2] > ceiling(max(hy$bili)))
      ylimits[2] <- ceiling(max(hy$bili))
    ybreaks <- as.numeric(unlist(strsplit(yaxisopt[1], ",")))
    if (max(ybreaks) > ceiling(max(hy$bili)))
      ybreaks[which.max(ybreaks)] <- ceiling(max(hy$bili))
  }

  p <-
    ggplot(hy, aes(
      x = astalt,
      y = bili,
      colour = TRTVAR,
      label = text,
      key = key
    )) + geom_point(na.rm = T) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = xbreaks,
                       limits = xlimits,
                       labels = unlist(strsplit(xaxisopt[3], ","))) +
    scale_y_continuous(breaks = ybreaks,
                       limits = ylimits,
                       labels = unlist(strsplit(yaxisopt[3], ","))) +
    geom_hline(
      yintercept = as.numeric(yrefline[1]),
      color = yrefline[2],
      linetype = yrefline[3]
    ) +
    geom_vline(
      xintercept = as.numeric(xrefline[1]),
      color = xrefline[2],
      linetype = xrefline[3]
    ) +
    geom_hline(yintercept = 1,
               color = 'grey30',
               linetype = "solid") +
    geom_vline(xintercept = 1,
               color = 'grey30',
               linetype = "solid") +
    labs(x = xaxisopt[4], y = yaxisopt[4])
  
  sp <-
    p + annotate(
      geom = "text",
      x = ceiling(max(hy$astalt)) - 0.2,
      y = ceiling(max(hy$bili)),
      label = "Potential Hy's Law Cases"
    ) +
    annotate(
      geom = "text",
      x = ceiling(max(hy$astalt)) - 0.1,
      y = 0.1,
      label = "Temple's Corollary"
    ) +
    annotate(
      geom = "text",
      x = 0.4,
      y = ceiling(max(hy$bili)),
      label = "Gilberts Syndrome or Cholestasis"
    ) +
    annotate(
      geom = "text",
      x = 0.1,
      y = 0.1,
      label = "Normal"
    )
  
  output = ggplotly(p, tooltip = c("label"), source = "plot_output")
  output <- output %>%
    add_annotations(
      x = quad_lables$x,
      y = quad_lables$y,
      text = quad_lables$text1,
      showarrow = F
    ) %>%
    layout(legend = list(
      orientation = "h",
      x = 0.4,
      y = -0.2,
      size = 20
    ))
  
  return(list(
    ptly = output,
    plot = sp,
    data = hy_data
  ))
}

# adlb<-read_sas("C:/Users/vj06/OneDrive - Pfizer/Documents/R/Data/adlb1.sas7bdat")
# adlb_c<-read.csv("C:/Users/vj06/OneDrive - Pfizer/Documents/R/Data/adlb1.csv")
# datain<-adlb