#Input from data_processing
#Currently 2 tables at a time only
#one high level and one low leve term
adaeT9 <- function(
  datain,#data_processing data
  population,
  AE_Filter,
  ctrlgrp,#treat 1
  trtgrp,#treat 2
  ui_pt= "AEDECOD",#change getstats
  ui_soc="AEBODSYS",#change getstats
  ui_statistics="Risk Difference",
  ui_trttotalyn='N',
  ui_trtbign="Y",
  ui_pctdisp="TRT",
  ui_alpha=0.05,
  ui_cutoff=5,
  ui_sortopt="Ascending",
  ui_sortvar="Count"
  ){
  #add AETERM==ADVERSE EVENT or not
  if (ui_trttotalyn == "Y") {
    datain <- datain %>% bind_rows(mutate(datain, TRTVAR = "Total"))
  }

  TrtN <- datain %>% group_by(TRTVAR) %>%
    summarise(N = length(unique(USUBJID)))

  ## categorical for Lower Term
  countslt <- datain %>%
    group_by(across(all_of(c(
      "TRTVAR", ui_soc, ui_pt
    )))) %>%
    summarise(FREQ = length(unique(USUBJID)))
  

  ## categorical for Higher Term
  countsht <-   datain %>%
    group_by(across(all_of(c("TRTVAR", ui_soc)))) %>%
    summarise(FREQ = length(unique(USUBJID)))


  
  #Calculate percentage with percent by options
  if (ui_pctdisp == "TRT") {
    countslt1 <- countslt %>% inner_join(TrtN, by = "TRTVAR")
    countsht1 <- countsht %>% inner_join(TrtN, by = "TRTVAR")
  } else if (ui_pctdisp == "VAR") {
    countslt1 <- countslt %>% mutate(N = length(unique(USUBJID)))
    countsht1 <- countsht %>% mutate(N = length(unique(USUBJID)))
  } else if (ui_pctdisp == "HT") {
    countslt1 <- datain %>% group_by(across(all_of(ui_soc))) %>%
      summarise(N = length(unique(USUBJID))) %>% inner_join(countslt, .)
    countsht1 <- datain %>% group_by(across(all_of(ui_soc))) %>%
      summarise(N = length(unique(USUBJID))) %>% inner_join(countsht, .)
  }

  countslt1 <- countslt1 %>% ungroup() %>%
    mutate(PCT = (FREQ / N) * 100,
           CVALUE = paste0(FREQ, " (", round(PCT, 2), ")")) %>%
    rename('CAT' = all_of(ui_pt))
  

  countsht1 <- countsht1 %>% ungroup() %>%
    mutate(PCT = (FREQ / N) * 100,
           CVALUE = paste0(FREQ, " (", round(PCT, 2), ")")) %>%
    rename('CAT' = all_of(ui_soc))

  counts <-
    countslt1 %>% select(-all_of(ui_soc)) %>% bind_rows(countsht1) 

  #Getting Risk DIfference/Ratio from getstats:
  #Risk for High term
  riskht = GetStatistics_all(
    data = datain,
    summary_by = "Patients",
    review_by = "SOC",
    ctrlgrp = ctrlgrp,
    trtgrp = trtgrp,
    statistics = ui_statistics,
    alpha = ui_alpha,
    cutoff = ui_cutoff,
    sort_opt = ui_sortopt,
    sort_var = ui_sortvar
  )
  #Risk for Low Term
  risklt <- GetStatistics_all(
    data = datain,
    summary_by = "Patients",
    review_by = "PT",
    ctrlgrp = ctrlgrp,
    trtgrp = trtgrp,
    statistics = ui_statistics,
    alpha = ui_alpha,
    cutoff = 0,
    sort_opt = ui_sortopt,
    sort_var = ui_sortvar
  )

  pairs <- unique(risklt$trt_pair)
  risklabel=paste0(ui_statistics," (CI)")

  #Get combined tables

  trts <- unique(risklt$TRTVAR)

  riskhti <- riskht %>%
    select(all_of(c(ui_soc, "RiskCI", "N1"))) %>%
    distinct()
  risklti <- risklt %>%
    select(all_of(c(ui_soc, ui_pt, "RiskCI", "N1"))) %>%
    distinct()

  uniqHT1 = unique(riskhti[riskhti$N1 != 0, ][[ui_soc]])
  uniqHT <- c(uniqHT1,
              unique(riskhti[[ui_soc]])[!(unique(riskhti[[ui_soc]]) %in% uniqHT1)])
  
  #Block sorting:
  riskhti <- riskhti %>%
    mutate(blksrt = match(riskhti[[ui_soc]], uniqHT)) %>%
    select(-N1) %>% distinct()
  risklti <- risklti %>%
    mutate(blksrt = match(risklti[[ui_soc]], uniqHT)) %>% filter(!is.na(blksrt)) %>% #remove socs not in cutoff
    select(-N1) %>% distinct()
  

  #Within block sorting:
  risklti <- risklti %>% group_by(blksrt) %>%
    mutate(rowsrt = row_number()) %>% ungroup() %>% select(-all_of(ui_soc)) %>%
    rename('CAT' = all_of(ui_pt))
  riskhti <- riskhti %>% mutate(rowsrt = 0) %>%
    rename('CAT' = all_of(ui_soc))

  risks <- rbind(riskhti, risklti) %>%
    mutate(RiskCI = str_replace_all(RiskCI, c(
      "NaN" = "NE",
      "Inf" = "NE",
      "-Inf" = "NE"
    )))

  #Process counts table:
  if (ui_trtbign == "Y") {
    countsi <- counts %>% select(-N) %>%
      inner_join(TrtN, by = "TRTVAR") %>%
      mutate(TRTVAR = paste0(TRTVAR, " (N=", N, ") n(%)"))
  } else{
    countsi <- counts %>%
      mutate(TRTVAR = paste0(TRTVAR, " n(%)"))
  }

  #Pivot counts table:
  countsi <- countsi %>% select(CAT, TRTVAR, CVALUE) %>%
    pivot_wider(
      names_from = TRTVAR,
      values_from = CVALUE,
      values_fill = "0"
    )
  

  #Combined table:
  final <- inner_join(countsi, risks, by = "CAT") %>%
    arrange(blksrt, rowsrt)
  boldrows <- which(final$rowsrt == 0)

  finaldat <- final %>% select(-blksrt, -rowsrt) %>%
    rename(" " = "CAT", !!risklabel := "RiskCI")
  
  report <- finaldat %>% flextable() %>%
    fontsize(size = 7, part = "all") %>%
    bold(i = boldrows, j = 1) %>% 
    theme_box() %>%
    align(align = "center", part = "header") %>%
    autofit()

  #Report title and footnote
  if (is.null(AE_Filter)) AE_Filter=""
  AE_cond<-ifelse(
    length(AE_Filter) <= 1,
    AE_Filter,
    paste0(
      paste0(
        AE_Filter[-length(AE_Filter)], 
        collapse = ", "),
      " and ",
      AE_Filter[length(AE_Filter)]
      )
    )

  title<-paste0("Participants With ",AE_cond, " Adverse Events by System Organ Class and Preferred Term \n", 
                population, " population")
  footnote<-paste0("* n is the number of with ", AE_cond ," adverse events.\n",
     ui_statistics,"is shown between ", ctrlgrp, " and ", trtgrp )
  return(list(tout=report,dout =final,title=title,footnote=footnote))
  
}


# out=adaeT9(datain=input_df, #data_processing data
#            ctrlgrp=unique(input_df$TRTVAR)[1],
#            trtgrp=unique(input_df$TRTVAR)[-1],
#            ui_catlabel=", ",
#            ui_subgrpvar=NA, #always remain NA
#            ui_pt= "AEDECOD",
#            ui_soc="AEBODSYS",
#            ui_trttotalyn='N')
