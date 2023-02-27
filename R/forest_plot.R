options(dplyr.summarise.inform = F)
Forest_Plot <-
  function(dat,
           review_by,
           summary_by,
           statistics,
           xlims = c(0, 3),
           xref = 1,
           pvalcut=0.05) {
    #Set review by term
    if (review_by == "PT") {
      byterm = "AEDECOD"
    } else if (review_by == "SOC") {
      byterm = "AEBODSYS"
    }
    #Calculate number by each treatment and store:
    dat <- dat %>% 
      mutate(TRTTXT=paste0(TRTVAR," (N=",ifelse(N1==0,N2_Total,N1_Total),")"))
    groupvar = "TRTTXT"
    #For height of plot calculation:
    nterm = length(unique(dat[[byterm]]))
    adjh <- max(nterm * 20, 600)
    #Percentage ScatterPlot data preparation:
    dat_out <-
      dat %>% filter(TEST != Inf) %>% 
      group_by(across(all_of(c(byterm, groupvar)))) %>%
      mutate(
        pct = ifelse(N1 == 0, PCT2, PCT1),
        Percentage = paste0(gsub("Risk.*$", "", 
                                 hover_text),pct,"%\n",
                            TRTVAR),
        SOC = AEBODSYS,
        PT = ifelse(review_by == "PT", AEDECOD, "")
      ) %>% arrange(desc(row_number()))
    dat_out[[byterm]] <- fct_inorder(dat_out[[byterm]])
    
    ##-Dynamic Plot width - review to update later:
    ##-nwid=max(max(dat_out$PCT1),max(dat_out$PCT2))
    ##-adjw=ifelse(nwid<=40,600,ifelse(nwid<=60,800,ifelse(nwid<=80,1000,1300)))
    
    #colors for Scatter Points:
    ctrl=dat_out$TRTTXT[dat_out$N2==0] %>% unique()
    trtlevels=unique(dat_out$TRTTXT[order(dat_out$TRTTXT!=ctrl,dat_out$TRTTXT)])
    trtcols=c("black" ,"royalblue2", "goldenrod" , "orchid3", "brown","pink")
    trtcols=setNames(trtcols[1:length(trtlevels)],trtlevels)
    #Shapes for Scatter Points:
    trtshapes=c(16,17,15,18,19)
    trtshapes=setNames(trtshapes[1:length(trtlevels)],trtlevels)
    
    ##Key for listings:
    dat_out$key <- row.names(dat_out)
    
    ## Significant points above or below control baseline 
    # to show - or + effect:
    baseline <- dat_out %>% ungroup() %>% filter(N2==0) %>% 
      select(all_of(c(byterm,"PCT1"))) %>% distinct() %>% 
      rename('PBL'='PCT1') 
    dat_out <- dat_out %>% ungroup() %>% 
      inner_join(baseline,by=byterm)
    dat1<<-dat_out
    #Identifying significant points:
    hltpts <- dat_out %>% filter(N1==0,pvalue<pvalcut) %>% 
      mutate(effect=ifelse(PCT2>PBL,"Significantly Higher",
                           ifelse(PCT2<PBL,"Significantly Lower",""))) %>% 
      filter(effect!="")
    #Color green or red for + and - effects
    hltfill <- setNames(c("red","green"),c("Significantly Higher","Significantly Lower"))
  #To Draw line between each term in all plots:
    liney=seq(1.5, length(unique(dat_out[[byterm]]))-0.5, 1)
    
    ### ScatterPlot creation
    sp = ggplot(
      data=dat_out,
      aes_string(
        x = "pct",
        y = byterm,
        color = groupvar,
        shape = groupvar,
        text = "Percentage",
        key = "key"
      )
    ) +
      geom_point(size = 0.9, position = "dodge") +
      scale_color_manual(name = "", 
                         values = trtcols) +
      scale_shape_manual(name = "", values = trtshapes) + 
      theme_bw() +
      scale_y_discrete() +
      geom_hline(yintercept=liney,linetype="dotted",
                 color="black",size=0.2,alpha=0.5)+
      scale_x_continuous(position = "top") +
      geom_point(data=hltpts,
                 aes_string(x="pct",y = byterm,fill="effect",
                            text = "Percentage",key="key")
                 ,inherit.aes = FALSE,shape=23,size=1.8,stroke=0.2)+
      scale_fill_manual(name="",values = hltfill)+
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

    #for static:
    sp1 <- sp + theme(legend.position = "bottom", legend.direction = "horizontal")
  #For interactive
    if (review_by == "PT") {
      splotly = ggplotly(
        sp,
        tooltip = c("text"),
        height = adjh,
        source = "plot_output"
      )
    } else{
      splotly = ggplotly(
        sp,
        tooltip = c("text"),
        height = adjh,
        source = "plot_output"
      )
    }

    #Fixing legend with parantheses
    for (i in 1:length(splotly$x$data)){
      if (!is.null(splotly$x$data[[i]]$name)){
splotly$x$data[[i]]$name = sub("\\(","",
                               str_split(splotly$x$data[[i]]$name,",")[[1]][1])
}}
    splotly <- splotly %>%
      layout(xaxis = list(side = "top"))
    
    
    ###Prepare Data for forest plot:
    #Risk values
    dat_out <- dat_out %>%
      mutate(hover_text=sub("n of.*Risk","Risk",hover_text))
    #Line/forest plot of risk ratio:

    fp = ggplot(
      dat_out,
      aes_string(
        y = byterm,
        x = "TEST",
        xmin = "TESTCIL",
        xmax = "TESTCIU",
        text = "hover_text",
        group="trt_pair",color = "trt_pair"
      )
    ) +
      geom_errorbarh(height = 0.1,
                     position = position_dodgev(height = 0.6),size=0.5) +
      geom_point(shape = 22,
                 size = 0.6,
                 position = position_dodgev(height = 0.6)) +
      geom_vline(xintercept = xref, linetype = 3) +
      geom_hline(yintercept=liney,linetype="dotted",color="black",size=0.2,alpha=0.5)+
      labs(x=statistics,color=NULL)+
      scale_y_discrete() +
      coord_cartesian(xlim = c(0,5)) +
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
    ##for download:
    fp1 = fp + theme(legend.position = "bottom", legend.direction = "horizontal")
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
    
  if(all(dat_out[[byterm]]==toupper(dat_out[[byterm]]))){
    fsize=2.2
  }else{fsize=2.4}
    termtable <- base +
      xlab(review_by) +
      geom_text(aes_string(y = byterm, 
                           label = byterm, text = byterm),
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

    ptable <-
      base +  scale_x_discrete(position = "top") +
      geom_text(
        aes_string(x="trt_pair",y = byterm, label = "pvalue",color="trt_pair"),
        size = 2.4,
        hjust = 0
      )+ xlab("p-value") +
      theme(legend.position="none")
    
    #Plotly P value table
    tplotly = ggplotly(ptable, height = adjh, 
                       source = "plot_output",showlegend = F) %>% 
      layout(xaxis =list(side="top")) %>%
      style(hoverinfo = "none", textposition = "middle center")
    
    ###Combine interactive plots into subplots for display
    inter_fig <- subplot(
      termplotly,
      splotly,
      fplotly,
      tplotly,
      widths = c(0.22, 0.32, 0.33, 0.13),
      titleX = TRUE,
      margin = 0.003
    ) %>%
      layout(showlegend = T,width=1300,
             legend = list(
             orientation = "h",
             x = 0.5,
             y = -0.1,
             yanchor="top",
             xanchor="center",
             size = 8,font=list(size=8)
             ))#,
    print("check 2")
    inter_fig$x$source <- "plot_output"
    
    ###Combine for For static ggplot output for download purposes:
    row1 = plot_grid(
      termstat,
      sp + theme(legend.position = "none"),
      fp+ theme(legend.position = "none"),
      ptable,
      align = "h"
      ,
      nrow = 1,
      rel_widths = c(2, 3.5, 3.5, 1)
    )
    print("check 3")
    #Separate legend from scatterplot:
    leg1 = get_legend(sp1)
    leg2=get_legend(fp1)
    leg=plot_grid(leg1,leg2,nrow=1,rel_widths = c(0.4,0.6))
    #Combine with legend:
    static_fig = plot_grid(
      row1,
      leg,
      nrow = 2,
      ncol = 1,
      rel_heights = c(0.9, 0.1)
    )
    print("check 4")
    #Data to be returned:
    out <- dat_out %>%
      select(all_of(c(byterm, groupvar)),
             pct,
             Percentage,
             TEST,
             TESTCIL,
             TESTCIU,
             pvalue,
             SOC)
    if (review_by == "PT") {
      out = out %>% rename(AEBODSYS = SOC)
    } else if (review_by == "SOC") {
      out = out %>% select(-SOC)
    }
    print("check 5")
    return(list(
      ptly = inter_fig,
      plot = static_fig,
      drill_plt = sp,
      data = out,
      n = nterm
    ))
  }
