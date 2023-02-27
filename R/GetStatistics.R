################################################################################
# GetStatistics.R
# 5 Functions: Obtain statistics for further TLG outputs.
################################################################################
options(warn = -1)

GetStatistics_all  = function(data,
                              summary_by,
                              review_by,
                              ctrlgrp,
                              trtgrp,
                              statistics,
                              alpha,
                              cutoff=NA,
                              sort_opt,
                              sort_var) {
  #print
  if (is.null(ctrlgrp) | is.null(trtgrp)) {
    return(NULL)
  }
  trtgrp=unlist(strsplit(trtgrp,","))
  #print(paste("Treatment is",trtgrp))
  getstats1<-data.frame()

  for (t in trtgrp){
    treatment1=ctrlgrp
    treatment2=t
  #print(treatment2)
  ### Filtering based on selected treatment groups -----------------------------
  data1 <- data %>% 
      filter(TRTVAR %in% c(treatment1, treatment2)) %>%
      mutate(trt_pair=paste0(treatment1," -vs- ",treatment2))
  
  ## Calculate Totals using ADSL
  N1_total <- data1 %>%
    {
      if (summary_by == "Patients")
        drop_na(., RFSTDTC)
      else
        .
    } %>%
    {
      if (summary_by == "Events")
        filter(., !is.na(AESTDT) | !is.na(AEDECOD))
      else
        .
    } %>%
    filter(TRTVAR %in% treatment1) %>%
    {
      if (summary_by == "Patients")
        distinct(., USUBJID)
      else
        .
    } %>%
    tally() %>% as.numeric()
  
  N2_total <- data1 %>%
    {
      if (summary_by == "Patients")
        drop_na(., RFSTDTC)
      else
        .
    } %>%
    {
      if (summary_by == "Events")
        filter(., !is.na(AESTDT) | !is.na(AEDECOD))
      else
        .
    } %>%
    filter(TRTVAR %in% treatment2) %>%
    {
      if (summary_by == "Patients")
        distinct(., USUBJID)
      else
        .
    } %>%
    tally() %>% as.numeric()
  
  
  if (nrow(data1) == 0) {
    return(NULL)
  }
  
  data_summary <- data1 %>%
    {
      if (review_by == "PT")
        group_by(., trt_pair,AEDECOD)
      else
        .
    } %>%
    {
      if (review_by == "SOC")
        group_by(., trt_pair,AEBODSYS)
      else
        .
    } %>%
    {
      if (summary_by == "Patients")
        distinct(., USUBJID, .keep_all = TRUE)
      else
        .
    } %>%
    {
      if (review_by == "PT")
        summarise(
          .,
          AEBODSYS = dplyr::first(AEBODSYS),
          COUNT1 = sum(TRTVAR %in% treatment1),
          COUNT2 = sum(TRTVAR %in% treatment2)
        )
      else
        .
    } %>%
    {
      if (review_by == "SOC")
        summarise(
          .,
          COUNT1 = sum(TRTVAR %in% treatment1),
          COUNT2 = sum(TRTVAR %in% treatment2)
        )
      else
        .
    } %>%
    na.omit()
  
  PT_N <- data1 %>%
    filter(!is.na(AEDECOD) & !is.na(RFSTDTC)) %>%
    group_by(AEDECOD, TRTVAR,trt_pair) %>%
    {
      if (summary_by == "Patients")
        summarise(., N = length(unique((USUBJID))))
      else
        .
    } %>%
    {
      if (summary_by == "Events")
        summarise(., N = n())
      else
        .
    }
  data_pt <<- PT_N
  
  SOC_N <- data1 %>%
    filter(!is.na(AEDECOD) & !is.na(RFSTDTC)) %>%
    group_by(AEBODSYS, TRTVAR,trt_pair) %>%
    {
      if (summary_by == "Patients")
        summarise(., N = length(unique((USUBJID))))
      else
        .
    } %>%
    {
      if (summary_by == "Events")
        summarise(., N = n())
      else
        .
    }
  data_soc <<- SOC_N
  
  
  ### Risk Difference (Large Sample Approximation by CLT) ------------------------
  if (statistics == "Risk Difference") {
    getRiskDifference <- function(count1, count2) {
      temp_N1 = N1_total - count1
      temp_N2 = N2_total - count2
      
      Rmat = matrix(c(temp_N1, temp_N2, count1, count2), nrow = 2)
      rd = riskdiff_wald(Rmat, conf.level = 1 - alpha)
      
      RD = round(rd$measure[2, 1], 3)
      RDP = round(rd$p.value[2, 3], 4)
      RDCIL = round(rd$measure[2, 2], 4)
      RDCIU = round(rd$measure[2, 3], 4)
      
      output <- c(RD, RDP, RDCIL, RDCIU)  #c(riskvalue, pvalue)
      return(output)
    }
    
    result <- data_summary %>%
      {
        if (review_by == "PT")
          group_by(.,trt_pair, AEDECOD)
        else
          .
      } %>%
      {
        if (review_by == "SOC")
          group_by(.,trt_pair, AEBODSYS)
        else
          .
      } %>%
      {
        if (review_by == "PT")
          summarise(
            .,
            AEBODSYS = dplyr::first(AEBODSYS),
            RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                           collapse = " ")
          )
        else
          .
      } %>%
      {
        if (review_by == "SOC")
          summarise(.,
                    RESULT = paste(getRiskDifference(COUNT1, COUNT2),
                                   collapse = " "))
        else
          .
      } %>%
      rowwise() %>%
      mutate(
        TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
        TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2]),
        TESTCIL = as.numeric(unlist(strsplit(RESULT, " "))[3]),
        TESTCIU = as.numeric(unlist(strsplit(RESULT, " "))[4]),
        N1_Total = N1_total,
        N2_Total = N2_total
      ) %>%
      select(-RESULT) %>%
      mutate(LOGPTEST = -log(TESTP))
    
    
    ### Risk Ratio (Large Sample Approximation by CLT) -----------------------------
  } else if (statistics == "Risk Ratio") {
    # NEED N1 AND N2
    getRiskRatio <- function(count1, count2) {
      temp_N1 = N1_total - count1
      temp_N2 = N2_total - count2
      
      Rmat = matrix(c(temp_N1, temp_N2, count1, count2), nrow = 2)
      rr <-
        riskratio.wald(Rmat, conf.level = 1 - alpha)  #calculating risk ratio
      RR = round(rr$measure[2, 1], 4)
      RRP = round(rr$p.value[2, 2], 4)
      RRCIL = round(rr$measure[2, 2], 4)
      RRCIU = round(rr$measure[2, 3], 4)
      
      output <- c(RR , RRP, RRCIL, RRCIU)
      return(output)
    }
    
    result <- data_summary %>%
      {
        if (review_by == "PT")
          group_by(.,trt_pair, AEDECOD)
        else
          .
      } %>%
      {
        if (review_by == "SOC")
          group_by(., trt_pair,AEBODSYS)
        else
          .
      } %>%
      {
        if (review_by == "PT")
          summarise(
            .,
            AEBODSYS = dplyr::first(AEBODSYS),
            RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                           collapse = " ")
          )
        else
          .
      } %>%
      {
        if (review_by == "SOC")
          summarise(.,
                    RESULT = paste(getRiskRatio(COUNT1, COUNT2),
                                   collapse = " "))
        else
          .
      } %>%
      rowwise() %>%
      mutate(
        TEST = as.numeric(unlist(strsplit(RESULT, " "))[1]),
        TESTP = as.numeric(unlist(strsplit(RESULT, " "))[2]),
        TESTCIL = as.numeric(unlist(strsplit(RESULT, " "))[3]),
        TESTCIU = as.numeric(unlist(strsplit(RESULT, " "))[4]),
        N1_Total = N1_total,
        N2_Total = N2_total
      ) %>%
      select(-RESULT) %>%
      mutate(LOGPTEST = -log(TESTP))
  }
  risk_results <<- result
  ### Integration of data for the purpose of graph & listing presentation ------
  

  result1 <- result %>% group_by() %>%
    #inner_join(data %>% select(TRTVAR, AEBODSYS) %>% unique()) %>%
    {
      if (review_by == "SOC")
        inner_join(., SOC_N)
      else
        .
    } %>%
    {
      if (review_by == "PT")
        inner_join(., PT_N)
      else
        .
    } %>%
    mutate(
      N1 = (TRTVAR %in% treatment1) * N,
      N2 = (TRTVAR %in% treatment2) * N,
      PCT1 = round((((TRTVAR %in% treatment1) * N
      ) / N1_total) * 100, 2),
      PCT2 = round((((TRTVAR %in% treatment2) * N
      ) / N2_total) * 100, 2),
      pvalue = TESTP,
      adjpvalue = p.adjust(pvalue, method = "fdr"),
      RiskCI=paste0(round(TEST, 3),
                    " (",
                    round(TESTCIL, 2),
                    ",",
                    round(TESTCIU, 2),
                    ")")
    )
  if (review_by == "SOC") {
    result2 <- result1 %>%
      mutate(
        hover_text = paste0(
          "\n SOC:",
          AEBODSYS,
          "\n n of ",
          ifelse(summary_by=="Patients","participants",summary_by),
          "= ",
          ifelse(N1 == 0, N2, N1),
          "\n",
          paste(statistics,"(CI)"),
          ":",
          RiskCI,
          "\n p-value:",
          round(pvalue, 4)
        )
      )
  }
  if (review_by == "PT") {
    result2 <- result1 %>%
      mutate(
        hover_text = paste0(
          "\n SOC:",
          AEBODSYS,
          "\n PT:",
          AEDECOD,
          "\n n of ",
          ifelse(summary_by=="Patients","participants",summary_by),
          "= ",
          ifelse(N1 == 0, N2, N1),
          "\n",
          paste(statistics,"(CI)"),
          ":",
          RiskCI,
          "\n p-value:",
          round(pvalue, 4)
        )
      )
  }
  if (!is.na(cutoff)){
  cutoff_result <-
    result2 %>% filter(PCT1 > cutoff | PCT2 > cutoff)
  }else{
    cutoff_result <- result2
    } 
  cutoff_result <- cutoff_result %>%
    {
      if (review_by == "SOC")
        distinct(., AEBODSYS)
      else
        .
    } %>%
    {
      if (review_by == "PT")
        distinct(., AEBODSYS, AEDECOD)
      else
        .
    }
  
  if (review_by == "SOC") {
    getstats <- result2 %>% inner_join(cutoff_result, by = c('AEBODSYS'))
  } else if (review_by == "PT") {
    getstats <-
      result2 %>% inner_join(cutoff_result, by = c('AEBODSYS', 'AEDECOD'))
  }
  

  if (nrow(getstats1)==0) {
    getstats1=getstats
  }else{
    getstats1=rbind(getstats1,getstats)
    }
  }
  
  notctrl <- getstats1 %>% filter(N1==0)
  getstats1 <- getstats1 %>% ungroup() %>% 
    {
      if (sort_opt == "Ascending"){  
        if (sort_var=="Count"){
          arrange(., N1) 
        }else if(sort_var=="Percent"){
          arrange(., PCT1)
        }else if(sort_var=="RiskValue"){
          arrange(., TEST)
        }
      }else if (sort_opt == "Descending"){
        if (sort_var=="Count"){
          arrange(., desc(N1)) %>% filter(N1!=0) %>% rbind(notctrl,.)
        }else if(sort_var=="Percent"){
          arrange(., desc(PCT1)) %>% filter(N1!=0) %>% rbind(notctrl,.)
        }else if(sort_var=="RiskValue"){
          arrange(., desc(TEST))
        }
      }else if (sort_opt == "Alphabetical"){
        if(review_by == "PT"){
          arrange(., AEDECOD)
        }else if(review_by == "SOC"){
          arrange(., AEBODSYS)
        }
      }else
        .
    }
  
  full_set <<- getstats1
  #cutoff_set <<- getstats
  
  return(getstats1)
  
}
