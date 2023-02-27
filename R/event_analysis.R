#' @title To generate event graphs for FDA Medical queries
#'
#' @param datain input dataset 
#' @param query_var MedDRA queries variable; valid value : FMQ
#' @param query_val MedDRA queries name captured in the MedDRA queries variable, One selection 
#' @param query_scope scope of the MedDRA queries; valid value : Narrow or Broad
#' @param pt_val Preferred Term valuet
#' @param ref_line y axis position of the Horzondal reference line 
#'
#' @return generated the normal plot and interactive plotly
#' @export
#'
#' @examples
#' event_analysis<-function(
#' datain,
#' query_var='FMQ_NAM',
#' query_val="erythema",
#' query_scope='Narrow',
#' pt_val="erythema",
#' ref_line=2
#'  )
#'  
event_analysis<-function(
  datain,
  query_var='FMQ_NAM',
  query_val="erythema",
  query_scope='Narrow',
  pt_val="erythema",
  ref_line=2
){
  names(datain) <- toupper(names(datain))
  
  dsin <- datain %>% mutate(QUERY_VAR=toupper(eval(parse(text=query_var))))
  
  # Sub setting the Query level an PT level data
  query_scope1 <- if (query_scope=='BROAD') c('BROAD','NARROW') else 'NARROW'
  
  query_val <- if (!is.null(query_val)) {str_to_upper(query_val)} else {stop('query_val is empty')}
  
  # Sub setting the Query level an FMQ level data
  dsin_query <-dsin %>% 
    filter(str_detect(QUERY_VAR, paste(paste0(query_val,"/",query_scope1),collapse="~~")))
  
  dsin_pt <-dsin %>% 
    filter(toupper(AEDECOD) == toupper(pt_val))
  
  # Calculating Treatment level N count
  Ncount <- datain %>% distinct(USUBJID, TRTVAR) %>% 
    group_by(TRTVAR) %>% 
    summarise(Nval=n_distinct(USUBJID))
  
  # Calculating query level subject count and % by PT grouping
  query_summary <- dsin_query %>% 
    group_by(TRTVAR,AEDECOD) %>% 
    summarise(nval=n_distinct(USUBJID)) %>% 
    left_join(Ncount, by="TRTVAR") %>% ungroup() %>% 
    mutate(pct=round((nval/Nval)*100,2),Percent=paste(pct,"% \n PT:",AEDECOD)) %>% 
    group_by(TRTVAR) %>% 
    mutate(DECODh=ifelse(toupper(AEDECOD) == toupper(pt_val),9999,rank(pct)))
  
  #Getting Max PCT value for stacked plot to have same scale for both plots
  yaxis_max<-query_summary %>% 
    group_by(TRTVAR) %>% 
    summarise(pct_s=sum(pct)) 
  yscal_max<-ifelse(max(yaxis_max$pct_s)<5,max(yaxis_max$pct_s)+10,max(yaxis_max$pct_s)+5)
  yscal_br<-ifelse(yscal_max>40,5,2)
  # Calculating pt level subject count and % 
  pt_summary <-dsin_pt %>% 
    group_by(TRTVAR) %>% 
    summarise(nval=n_distinct(USUBJID)) %>% 
    left_join(Ncount, by="TRTVAR") %>% 
    mutate(pct=round((nval/Nval)*100,2),Percent=paste(pct,"%")) %>% 
    arrange(TRTVAR,pct)
  
  # Generating bar chart for PT level
  pt_plot<-pt_summary %>% 
    ggplot(aes(x=TRTVAR, y=pct,text=Percent))+
    geom_bar(stat='identity',width=0.4, fill="royalblue3",color="black",size=0.4)+
    scale_y_continuous(limits=c(0,yscal_max),breaks=seq(0,yscal_max,yscal_br))+
    labs(x="Treatment",y="Percentage of Participants",colour = NULL)+
    geom_hline(yintercept = ref_line,linetype = "dashed")+
    theme_bw()+
    theme(axis.text.x = element_text(angle =15, hjust = 1,size=8),
          axis.text.y = element_text(size = 6),
          plot.title = element_text(size=8),
          legend.text = element_text(size=5),
          plot.margin=unit(c(1, 0, 0, 1.5),"cm"))
  
  pt_plot1 <- pt_plot +ggtitle(paste0(str_to_title(pt_val)," PT"))
  pt_ptly <- ggplotly(pt_plot,tooltip="text") %>% 
    add_annotations(text=paste0(str_to_title(pt_val)," PT"),
                    x=0.5,
                    y=1,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "center",
                    yanchor = "top",
                    showarrow=FALSE,
                    yshift=35,
                    font=list(size=12))
  
  # Generating bar chart for query level
  all_cols=grDevices::colors()[grep('gr(a|e)|royalblue', grDevices::colors(), invert = T)]
  #sample(all_cols,length(unique(query_summary$AEDECOD)))
  manualcolors<-c("royalblue3", 
                  all_cols[seq(length(all_cols),3,
                               length.out=length(unique(query_summary$AEDECOD)))])
  query_plot<-query_summary %>% 
    ggplot(aes(x=TRTVAR, y=pct,fill=reorder(AEDECOD,-DECODh),group=DECODh,text=Percent))+
    geom_bar(position = "stack",stat="identity",width=0.5,color="black",size=0.4)+
    scale_y_continuous(limits=c(0,yscal_max),breaks=seq(0,yscal_max,yscal_br))+
    scale_fill_manual(values=manualcolors)+
    labs(x="Treatment",y="Percentage of Participants",colour = NULL)+
    geom_hline(yintercept = ref_line,linetype = "dashed")+
    guides(fill=guide_legend(title=NULL))+
    theme_bw()+
    theme(axis.text.x = element_text(angle =15, hjust = 1,size=8),
          axis.text.y = element_text(size = 6),
          plot.title = element_text(size=8),
          legend.text = element_text(size=5),
          plot.margin=unit(c(1, 0, 0, 1.5),"cm"))
  query_plot1 <- query_plot + ggtitle(str_wrap(paste0(toupper(sub("\\_.*", "", query_var)),
                                                      " Categorization of ",str_to_title(query_val)),width=80))
  query_ptly <- ggplotly(query_plot,tooltip = "text") %>% 
    add_annotations(text=str_wrap(paste0(toupper(sub("\\_.*", "", query_var)),
                                         " Categorization of ",str_to_title(query_val)),width=80),
                    x=0.5,
                    y=1,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "center",
                    yanchor = "top",
                    showarrow=FALSE,
                    yshift=35,
                    font=list(size=12))
  
  fig_ptly <- subplot(pt_ptly,query_ptly,
                      titleY = T,shareX = T,shareY = T,
                      widths = c(0.5,0.5),margin = 0.005) %>% 
    layout(showlegend = T)
  
  fig_plt <- plot_grid(pt_plot1,query_plot1,nrow=1,
                       align = "h",
                       rel_widths = c(4,6))
  # return list of plots
  return(list(plot=fig_plt,ptly=fig_ptly))
}




