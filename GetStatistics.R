################################################################################
# GetStatistics.R
# 5 Functions: Obtain statistics for further TLG outputs.
################################################################################
options(warn=-1)

GetStatistics_all  = function(data,summary_by, review_by, treatment1, treatment2, statistics,alpha,cutoff,sort_opt) {
  #print
  
  if (is.null(treatment1) | is.null(treatment2)) {return(NULL)}
  
  ### Filtering based on selected treatment groups -----------------------------
  data <- data %>% filter(ARM %in% c(treatment1, treatment2))
  ## Calculate Totals using ADSL
  N1_total <- data %>%
    {if (summary_by == "Patients") drop_na(.,RFSTDTC) else .} %>%
    {if (summary_by == "Events") filter(.,!is.na(AESTDT) | !is.na(AEDECOD)) else .} %>%
    filter(ARM %in% treatment1) %>%
    {if (summary_by == "Patients") distinct(.,USUBJID) else .} %>% 
    tally() %>% as.numeric()
  
  N2_total <- data %>%
    {if (summary_by == "Patients") drop_na(.,RFSTDTC) else .} %>%
    {if (summary_by == "Events") filter(.,!is.na(AESTDT) | !is.na(AEDECOD)) else .} %>%
    filter(ARM %in% treatment2) %>%
    {if (summary_by == "Patients") distinct(.,USUBJID) else .} %>% 
    tally() %>% as.numeric()
  
  
  if (nrow(data) == 0) {return(NULL)}
  
  data_summary <- data %>%
    {if (review_by == "PT") group_by(., AEDECOD) else .} %>%
    {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
    {if (summary_by == "Patients") distinct(.,USUBJID, .keep_all = TRUE) else .} %>%
    {if (review_by == "PT") summarise(.,
                                      AEBODSYS = dplyr::first(AEBODSYS),
                                      COUNT1 = sum(ARM %in% treatment1),
                                      COUNT2 = sum(ARM %in% treatment2)) else .} %>%
    {if (review_by == "SOC") summarise(.,
                                       COUNT1 = sum(ARM %in% treatment1),
                                       COUNT2 = sum(ARM %in% treatment2)) else .} %>%
    na.omit()
  
  PT_N <- data %>%
    filter(!is.na(AEDECOD) & !is.na(RFSTDTC)) %>%
    group_by(AEDECOD, ARM) %>%
    {if (summary_by == "Patients") summarise(., N = length(unique((USUBJID)))) else .} %>%
    {if (summary_by == "Events") summarise(., N = n()) else .}
  data_pt<<-PT_N
  
  SOC_N <- data %>%
    filter(!is.na(AEDECOD) & !is.na(RFSTDTC)) %>%
    group_by(AEBODSYS, ARM) %>%
    {if (summary_by == "Patients") summarise(., N = length(unique((USUBJID)))) else .} %>%
    {if (summary_by == "Events") summarise(., N = n()) else .}
  data_soc<<-SOC_N 
  
  
  ### Risk Difference (Large Sample Approximation by CLT) ------------------------
  if (statistics=="Risk Difference"){
    getRiskDifference <- function(count1, count2) {
      
      temp_N1 = N1_total - count1
      temp_N2 = N2_total - count2
      
      Rmat = matrix(c(temp_N1,temp_N2, count1, count2 ),nrow= 2 )  
      rd = riskdiff_wald(Rmat,conf.level= 1-alpha)
      
      RD = round(rd$measure[2,1],3)
      RDP = round(rd$p.value[2,3],4)
      RDCIL=round(rd$measure[2,2],4)
      RDCIU=round(rd$measure[2,3],4)
      
      output <- c(RD, RDP,RDCIL,RDCIU)  #c(riskvalue, pvalue)
      return(output)
    }
    
    result <- data_summary %>%
      {if (review_by == "PT") group_by(., AEDECOD) else .} %>%
      {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
      {if (review_by == "PT") summarise(.,
                                        AEBODSYS = dplyr::first(AEBODSYS),
                                        RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                                       collapse = " ")) else .} %>%
      {if (review_by == "SOC") summarise(.,
                                         RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                                        collapse = " ")) else .} %>%
      rowwise() %>%
      mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
             TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2]),
             TESTCIL = as.numeric(unlist(strsplit(RESULT, " "))[3]),
             TESTCIU = as.numeric(unlist(strsplit(RESULT, " "))[4]),
             N1_Total=N1_total,
             N2_Total=N2_total) %>%
      select(-RESULT) %>%
      mutate(LOGPTEST = -log(TESTP))
    
    
    ### Risk Ratio (Large Sample Approximation by CLT) -----------------------------
  } else if (statistics=="Risk Ratio"){
    # NEED N1 AND N2
    getRiskRatio <- function(count1, count2) {
      
      temp_N1 = N1_total - count1
      temp_N2 = N2_total - count2
      
      Rmat = matrix(c(temp_N1,temp_N2, count1, count2 ),nrow= 2 )
      rr <- riskratio.wald(Rmat,conf.level= 1-alpha)  #calculating risk ratio
      RR = round(rr$measure[2,1],4)
      RRP = round(rr$p.value[2,2],4)
      RRCIL=round(rr$measure[2,2],4)
      RRCIU=round(rr$measure[2,3],4)
      
      output <- c(RR , RRP,RRCIL,RRCIU)
      return(output)
    }
    
    result <- data_summary %>%
      {if (review_by == "PT") group_by(., AEDECOD) else .} %>%
      {if (review_by == "SOC") group_by(., AEBODSYS) else .} %>%
      {if (review_by == "PT") summarise(.,
                                        AEBODSYS = dplyr::first(AEBODSYS),
                                        RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                                       collapse = " ")) else .} %>%
      {if (review_by == "SOC") summarise(.,
                                         RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                                        collapse = " ")) else .} %>%
      rowwise() %>%
      mutate(TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
             TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2]),
             TESTCIL = as.numeric(unlist(strsplit(RESULT, " "))[3]),
             TESTCIU = as.numeric(unlist(strsplit(RESULT, " "))[4]),
             N1_Total=N1_total,
             N2_Total=N2_total) %>%
      select(-RESULT) %>%
      mutate(LOGPTEST = -log(TESTP))
  }
  risk_results<<-result
  ### Integration of data for the purpose of graph & listing presentation ------
  result1 <- result %>% group_by() %>%
    #inner_join(data %>% select(ARM, AEBODSYS) %>% unique()) %>% 
    {if (review_by == "SOC") inner_join(.,SOC_N) else .} %>%
    {if (review_by == "PT") inner_join(.,PT_N) else .} %>%
    mutate(N1 = (ARM %in% treatment1) * N,
           N2 = (ARM %in% treatment2) * N,
           PCT1 = round((((ARM %in% treatment1) * N)/N1_total)*100,2),
           PCT2 = round((((ARM %in% treatment2) * N)/N2_total)*100,2),
           pvalue = TESTP,
           adjpvalue = p.adjust(pvalue, method="fdr"))
  if(review_by=="SOC"){
    result2<-result1%>%
      mutate(hover_text = paste0("\n SOC:", AEBODSYS, 
                                 "\n No of ",summary_by,"= ",ifelse(N1==0,N2,N1),
                                 "\n", statistics, ":", TEST,
                                 "\n p-value:", round(pvalue,4)))
  }
  if(review_by=="PT"){
    result2<-result1%>%
      mutate(hover_text = paste0("\n SOC:", AEBODSYS,"\n PT:", AEDECOD, 
                                 "\n No of ",summary_by,"= ",ifelse(N1==0,N2,N1),
                                 "\n", statistics, ":", TEST,
                                 "\n p-value:", round(pvalue,4)))
  }
  
  cutoff_result <- result2 %>% filter(PCT1 > cutoff | PCT2 > cutoff)%>%
    {if(review_by=="SOC") distinct(.,AEBODSYS) else .}%>%
    {if(review_by=="PT") distinct(.,AEBODSYS,AEDECOD) else .}
  cutoff_events<<-cutoff_result
  
  
  if(review_by=="SOC"){
    getstats <- result2 %>% inner_join(cutoff_result, by=c('AEBODSYS'))
  } else if(review_by=="PT"){
    getstats <- result2 %>% inner_join(cutoff_result, by=c('AEBODSYS','AEDECOD'))
  }
  getstats<-getstats%>%        
    {if(sort_opt=="Ascending Count") arrange(.,N) else .}%>%
    {if(sort_opt=="Descending Count") arrange(.,desc(N)) else .}%>%
    {if(sort_opt=="Alphabetical" & review_by=="PT") arrange(.,AEDECOD) else .}%>%
    {if(sort_opt=="Alphabetical" & review_by=="SOC") arrange(.,AEBODSYS) else .}
  
  full_set<<-result2
  cutoff_set<<-getstats
  
  return(getstats)
  
}
