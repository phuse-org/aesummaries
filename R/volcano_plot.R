################################################################################
# volcano_plot.R
# 1 Function: Generation of volcano plot along with specifications.
################################################################################

volcano_plot <- function(data,
                         statistics_data,
                         statistics, 
                         treatment1,
                         treatment2, 
                         X_ref,
                         X_label= '',
                         review_by,
                         summary_by,
                         pvalue_label,
                         treatment1_label,
                         treatment2_label)
{
  
  ### Construction of volcano plot ---------------------------------------------
  if (nrow(statistics_data) == 0) {
    p <- ggplot() + 
      annotate("text", x = 4, y = 25, size = 8, label = "No data selected for plot.") + 
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())      
    return(list(plot = p, data = data))
  }
  
  statistics_data = statistics_data %>% filter(TEST!=Inf)
  
  key <- row.names(statistics_data)
  
  # Error when there are no adjusted p-values <= 0.05 so remove FDR adjusted P when no adjusted p <= 0.05---------------------
  check_sig <- statistics_data %>% group_by() %>% filter(adjpvalue<=0.05)
  
  if (nrow(check_sig) !=0) {
    
    pvalue_adj0.05 = (statistics_data %>% group_by() %>% filter(adjpvalue<=0.05) %>% arrange(desc(adjpvalue)) %>% slice(1))$pvalue
    
    p <- ggplot(statistics_data, aes(TEST, pvalue, label = hover_text, key = key, fill=AEBODSYS)) + # color code by SOC
      geom_point(aes(size=N), pch=21, alpha=0.5,) + 
      geom_hline(yintercept = 0.05, color = 'grey30', linetype = "dashed") +
      geom_hline(yintercept = pvalue_adj0.05, color = 'grey30', linetype = "dotted") +
      geom_vline(xintercept = ifelse(grepl("Ratio",statistics),1,0), color = 'grey30', linetype = "dashed") +
      geom_vline(xintercept = ifelse(grepl("Ratio",statistics),1,0)+c(-X_ref,X_ref), color = 'grey30', linetype = "dashed") +
      theme_classic() +
      background_grid(major = "xy", minor = "none", color.major="grey92")+
      guides(fill=guide_legend("System Organ Class",title.position="top"),size="none")+
      theme(legend.position = "bottom",legend.text=element_text(size=6))+
      scale_size_continuous(range = c(1, 8)) #+ ylim(0,1) #change
    
  } else{
    
    p <- ggplot(statistics_data, aes(TEST, pvalue, label = hover_text, key = key, fill=AEBODSYS)) + # color code by SOC
      geom_point(aes(size=N), pch=21, alpha=0.5) + 
      geom_hline(yintercept = 0.05, color = 'grey30', linetype = "dashed") +
      geom_vline(xintercept = ifelse(grepl("Ratio",statistics),1,0), color = 'grey30', linetype = "dashed") +
      geom_vline(xintercept = ifelse(grepl("Ratio",statistics),1,0)+c(-X_ref,X_ref), color = 'grey30', linetype = "dashed") +
      theme_classic() +
      background_grid(major = "xy", minor = "none", color.major="grey92")+
      guides(fill=guide_legend(title = "System Organ Class",title.position="top"),size="none")+
      theme(legend.position = "bottom",legend.text=element_text(size=6))+
      scale_size_continuous(range = c(1, 8))  
  }
  
  ### P value transformation-------------------------------------------------------------------------------
  if (pvalue_label=="-log10"){
    p=p+ scale_y_continuous("-log10(p-value)",
                            trans = reverselog_trans(10), 
                            breaks = as.numeric(paste0("1e-",0:20)), 
                            labels = as.character(0:20), 
                            expand = expansion(mult = c(0.05, 0.05)))
  } else if (pvalue_label=="None"){
    p=p+ scale_y_continuous("P-value",
                            trans = reverselog_trans(10), 
                            breaks = c(0.05, 0, .000000001,.00000001,.0000001,.000001,.00001,.00001, .0001, .001, .01, .1, 1),
                            labels = as.character(c(0.05, 0, .000000001,.00000001,.0000001,.000001,.00001,.00001, .0001, .001, .01, .1, 1)),
                            expand = expansion(mult = c(0.05, 0.05)))
    
  }
  
  text1 <- paste0("<- Favors ", treatment1_label,
                  " (N=", unique(statistics_data$N1_Total), ")")
  text2 <- paste0("Favors ", treatment2_label,
                  " (N=", unique(statistics_data$N2_Total), ") ->")
  
  lab1 = paste(statistics, treatment1_label, "vs.", treatment2_label)
  lab2 = paste(text1, text2)
  
  label_fin = paste0(lab2,"\n", lab1)
  p <- p +
    scale_x_continuous( label_fin,expand = expansion(mult = c(0.05, 0.05))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  output = ggplotly(p,tooltip = c("label"), source="plot_output",height=800)# %>%
  output$x$layout$legend$font$size=8.5 # change legend text size
  output$x$layout$legend$orientation="h"
  output$x$layout$legend$x= 0.4
  output$x$layout$legend$y=-0.2
  
  return(list(ptly=output,plot = p, data = data))
}  


