################################################################################
# server.R
# This R Script performs all data processing
################################################################################
data_processing<-function(datain,
                          domain,
                          data_source,
                          server_path,
                          data_filter,
                          obs_period,
                          obs_residual=NULL){
  # Reading data
  if (data_source=="Local"){
  if (is.null(datain)) return()
  if(file_ext(datain)=="csv"){
    #dsin<-fread(datain$datapath)
    dsin<-read.csv(datain$datapath)
  }
  if(file_ext(datain)=="sas7bdat"){
    dsin<-read_sas(datain$datapath)
  }
  }
  if (data_source=="Server"){
    dsin <-read_sas(paste0("/Volumes/app/cdars/prod/",server_path,"/saseng/cdisc3_0/data_vai/",
                           'ad',tolower(domain),'.sas7bdat'))
  }
  # processing data
  if(domain=="AE"){
    
  res1<<-obs_residual
  
  if(file_ext(datain)=="csv"){
    df1<-dsin%>%mutate(AESTDT = ifelse("AESTDT" %in% names(dsin),ifelse(is.character(AESTDT),as.Date(AESTDT,format="%d%b%Y"),AESTDT),
                                       ifelse(is.character(ASTDT),as.Date(ASTDT,format="%d%b%Y"),ASTDT)),
                       AEENDT = ifelse("AEENDT" %in% names(dsin),ifelse(is.character(AEENDT),as.Date(AEENDT,format="%d%b%Y"),AEENDT),
                                       ifelse(is.character(AENDT),as.Date(AENDT,format="%d%b%Y"),AENDT)),
                       RFSTDTC = ifelse("RFSTDTC" %in% names(dsin),ifelse(is.character(RFSTDTC),as.Date(RFSTDTC,format="%d%b%Y"),RFSTDTC),
                                        ifelse(is.character(TRTSDT),as.Date(TRTSDT,format="%d%b%Y"),TRTSDT)),
                       RFENDTC = ifelse("RFENDTC" %in% names(dsin),ifelse(is.character(RFENDTC),as.Date(RFENDTC,format="%d%b%Y"),RFENDTC),
                                        ifelse(is.character(TRTEDT),as.Date(TRTEDT,format="%d%b%Y"),TRTEDT)))
    }else if(file_ext(datain)=="sas7bdat"){
    df1<-dsin%>%mutate(AESTDT = ifelse("AESTDT" %in% names(dsin),ifelse(is.character(AESTDT),as.Date(AESTDT,format="%Y-%m-%d"),AESTDT),
                                        ifelse(is.character(ASTDT),as.Date(ASTDT,format="%Y-%m-%d"),ASTDT)),
                       AEENDT = ifelse("AEENDT" %in% names(dsin),ifelse(is.character(AEENDT),as.Date(AEENDT,format="%Y-%m-%d"),AEENDT),
                                        ifelse(is.character(AENDT),as.Date(AENDT,format="%Y-%m-%d"),AENDT)),
                       RFSTDTC = ifelse("RFSTDTC" %in% names(dsin),ifelse(is.character(RFSTDTC),as.Date(RFSTDTC,format="%Y-%m-%d"),RFSTDTC),
                                         ifelse(is.character(TRTSDT),as.Date(TRTSDT,format="%Y-%m-%d"),TRTSDT)),
                       RFENDTC = ifelse("RFENDTC" %in% names(dsin),ifelse(is.character(RFENDTC),as.Date(RFENDTC,format="%Y-%m-%d"),RFENDTC),
                                         ifelse(is.character(TRTEDT),as.Date(TRTEDT,format="%Y-%m-%d"),TRTEDT)))}

  
  df_out0<<-df1
  df1 <- df1 %>%
    drop_na(RFSTDTC) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),  ##aestdt=notmiss (adverse event started)and aedecod = miss
           AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
           AESEV=ifelse("AESEV" %in% (names(df1)),AESEV,
                        ifelse("ATOXGR" %in% (names(df1)),
                               recode(ATOXGR,"GRADE 0"="MILD",
                                      "GRADE 1"="MILD",
                                      "GRADE 2"="MODERATE",
                                      "GRADE 3"="MODERATE",
                                      "GRADE 4"="SEVERE",
                                      "GRADE 5"="SEVERE"))))  
  df_out<<-df1
  
  
  # filter data for seriousness, drug-related, and severity
  if (length(data_filter)>0){
    if ("Treatment emergent" %in% data_filter) { df1 <- df1 %>% filter(TRTEMFL == "Y")}
    if ("Serious" %in% data_filter) { df1 <- df1 %>% filter(AESER == "Y") }
    if ("Drug-related" %in% data_filter) { df1 <- df1 %>% filter(AEREL == "RELATED") }
    if (sum(c("Mild","Moderate","Severe") %in% data_filter)>0){
      severity_filter = data_filter[which(data_filter %in% c("Mild","Moderate","Severe"))]
      df1 = df1 %>% filter(AESEV %in% toupper(severity_filter))
    }
    if (sum(c("Recovered/Resolved","Recovering/Resolving","Not Recovered/Not Resolved","Fatal") %in% data_filter)>0){
      severity_filter = data_filter[which(data_filter %in% c("RECOVERED/RESOLVED","RECOVERING/RESOLVING","NOT RECOVERED/NOT RESOLVED","FATAL"))]
      df1 = df1 %>% filter(AEOUT %in% toupper(severity_filter))
    }
  }
  df_out1<<-df1
  
  # filter data for ae timeframe
  if (obs_period == "Overall Duration ") { df1 <- df1 %>% filter(STUDYFL == "Y") 
  } else if (obs_period == "Other") { df1 <- df1 %>% filter((AESTDT >= RFSTDTC) & (AESTDT < (RFENDTC + obs_residual))) }
  df_out2<<-df1
  }else if(domain=="LB"){
    df1<<-dsin
  }
  input_df<<-df1
  return(df1)
}