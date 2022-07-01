################################################################################
# server.R
# This R Script performs all data processing
################################################################################
data_processing<-function(datain,
                          domain,
                          data_source,
                          server_path,
                          data_filter,
                          trtvar,
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
  if(file_ext(datain)=="xpt"){
    dsin<-read_xpt(datain$datapath)
  }
  if (data_source=="Server"){
    dsin <-read_sas(paste0("/Volumes/app/cdars/prod/",server_path,"/saseng/cdisc3_0/data_vai/",
                           'ad',tolower(domain),'.sas7bdat'))
  }
  # processing data
  names(dsin) <- toupper(names(dsin))
  
  names(dsin)[names(dsin) == toupper(trtvar)] <- "TRTVAR"
  
  if(domain=="AE"){
    date_formats <- c('%d%b%Y', '%Y-%m-%d','%m/%d/%Y %H:%M')
    if ("AESTDT" %in% names(dsin)){
      df1 <- dsin %>% mutate(AESTDT=as.Date(AESTDT, tryFormats = date_formats, optional = F))
    }else{
      df1 <- dsin %>% mutate(AESTDT=as.Date(ASTDT, tryFormats = date_formats, optional = F))
    }
    if ("AEENDT" %in% names(dsin)){
      df1 <- df1 %>% mutate(AEENDT=as.Date(AEENDT, tryFormats = date_formats, optional = F))
    }else{
      df1 <- df1 %>% mutate(AEENDT=as.Date(AENDT, tryFormats = date_formats, optional = F))
    }
    if ("RFSTDTC" %in% names(dsin)){
      df1 <- df1 %>% mutate(RFSTDTC=as.Date(RFSTDTC, tryFormats = date_formats, optional = F))
    }else{
      df1 <- df1 %>% mutate(RFSTDTC=as.Date(TRTSDT, tryFormats = date_formats, optional = F))
    }
    if ("RFENDTC" %in% names(dsin)){
      df1 <- df1 %>% mutate(RFENDTC=as.Date(RFSTDTC, tryFormats = date_formats, optional = F))
    }else{
      df1 <- df1 %>% mutate(RFENDTC=as.Date(TRTEDT, tryFormats = date_formats, optional = F))
    }
  
  df_out0<<-df1
  df1 <- df1 %>%
    drop_na(RFSTDTC) %>%
    mutate(AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),  ##aestdt=notmiss (adverse event started)and aedecod = miss
           AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT))
  if ("AESEV" %in% (names(df1))){
    df1 <- df1 %>% mutate(AESEV=toupper(AESEV))
  }else if ("ATOXGR" %in% (names(df1))){
    df1 <- df1 %>% mutate(AESEV=recode(ATOXGR,"GRADE 0"="MILD",
                                       "GRADE 1"="MILD",
                                       "GRADE 2"="MODERATE",
                                       "GRADE 3"="MODERATE",
                                       "GRADE 4"="SEVERE",
                                       "GRADE 5"="SEVERE"))
  }
  
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