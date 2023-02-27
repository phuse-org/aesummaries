
#' This utility is for pre processing data for AE specific user inputs
#'
#' @param datain the ADam dataset read from the dataread utility is taken as input data
#' @param Population_Filter Population Filter for dataset
#' @param data_filter The filter UI elemments that are applied for the report is taken as input
#' @param trtvar Select the treatment variable 
#' @param obs_period applied for considering AE event that happened in specific duration
#' @param obs_residual Appled for additional period for consideration of AE
#' @param fmq_query_list the FMQ query list input file
#'
#' @return Returns a dataframe dout
#' @export
#'
#' @examples
#' adae<-read_sas("../data/adae.sas7bdat")
#' data_pre<-pre_processing(
#' datain=adae,
#' data_filter="Treatment emergent"
#' obs_period="Overall Duration",
#' obs_residual="30",
#' trtvar="TRTA",
#' Population_Filter ="Safety",
#' fmq_query_list = read.csv("../data/FMQ_Consolidated_List.csv")

data_processing <- function(datain,
                            Population_Filter,
                            data_filter,
                            trtvar,
                            obs_period,
                            obs_residual = NULL,
                            fmq_query_list) {
  dsin <- datain
  print("check1")
  # processing data
  names(dsin) <- toupper(names(dsin))
  names(dsin)[names(dsin) == toupper(trtvar)] <- "TRTVAR"
  date_formats <- c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y %H:%M")
  if ("AESTDT" %in% names(dsin)) {
    dsin <- dsin %>% mutate(AESTDT = as.Date(AESTDT, tryFormats = date_formats, optional = F))
  } else {
    dsin <- dsin %>% mutate(AESTDT = as.Date(ASTDT, tryFormats = date_formats, optional = F))
  }
  if ("AEENDT" %in% names(dsin)) {
    dsin <- dsin %>% mutate(AEENDT = as.Date(AEENDT, tryFormats = date_formats, optional = F))
  } else {
    dsin <- dsin %>% mutate(AEENDT = as.Date(AENDT, tryFormats = date_formats, optional = F))
  }
  if ("RFSTDTC" %in% names(dsin)) {
    dsin <- dsin %>% mutate(RFSTDTC = as.Date(RFSTDTC, tryFormats = date_formats, optional = F))
  } else {
    dsin <- dsin %>% mutate(RFSTDTC = as.Date(TRTSDT, tryFormats = date_formats, optional = F))
  }
  if ("RFENDTC" %in% names(dsin)) {
    dsin <- dsin %>% mutate(RFENDTC = as.Date(RFSTDTC, tryFormats = date_formats, optional = F))
  } else {
    dsin <- dsin %>% mutate(RFENDTC = as.Date(TRTEDT, tryFormats = date_formats, optional = F))
  }
  df0 <- dsin %>%
    drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD), ## aestdt=notmiss (adverse event started)and aedecod = miss
      AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT)
    )
  if ("AESEV" %in% (names(df0))) {
    df0 <- df0 %>% mutate(AESEV = toupper(AESEV))
  } else if ("ATOXGR" %in% (names(df0))) {
    df0 <- df0 %>% mutate(AESEV = recode(ATOXGR,
      "GRADE 0" = "MILD",
      "GRADE 1" = "MILD",
      "GRADE 2" = "MODERATE",
      "GRADE 3" = "MODERATE",
      "GRADE 4" = "SEVERE",
      "GRADE 5" = "SEVERE"
    ))
  }
  # Population Filter
  df1 <- df0 %>%
    {
      if (Population_Filter == "Safety") {
        filter(., SAFFL == "Y")
      } else if (Population_Filter == "Intent to Treat") {
        filter(., ITTFL == "Y")
      } else if (Population_Filter == "Modified Intent to Treat") {
        filter(., MITTFL == "Y")
      } else if (Population_Filter == "Per Protocol") {
        filter(., PPFL == "Y")
      } else if (Population_Filter == "Randomised") {
        filter(., RANDFL == "Y")
      } else {
        .
      }
    }

  # filter data for seriousness, drug-related, and severity
  if (length(data_filter) > 0) {
    if ("Treatment emergent" %in% data_filter) {
      df1 <- df1 %>% filter(TRTEMFL == "Y")
    }
    if ("Serious" %in% data_filter) {
      df1 <- df1 %>% filter(AESER == "Y")
    }
    if ("Drug-related" %in% data_filter) {
      df1 <- df1 %>% filter(AEREL == "RELATED")
    }
    if (sum(c("Mild", "Moderate", "Severe") %in% data_filter) > 0) {
      severity_filter <- data_filter[which(data_filter %in% c("Mild", "Moderate", "Severe"))]
      df1 <- df1 %>% filter(AESEV %in% toupper(severity_filter))
    }
    if (sum(c("Recovered/Resolved", "Recovering/Resolving", "Not Recovered/Not Resolved", "Fatal") %in% data_filter) > 0) {
      severity_filter <- data_filter[which(data_filter %in% c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED", "FATAL"))]
      df1 <- df1 %>% filter(AEOUT %in% toupper(severity_filter))
    }
  }

  # filter data for ae timeframe
  if (obs_period == "Overall Duration ") {
    df1 <- df1 %>% filter(STUDYFL == "Y")
  } else if (obs_period == "Other") {
    df1 <- df1 %>% filter((AESTDT >= RFSTDTC) & (AESTDT < (RFENDTC + obs_residual)))
  }


  # Reading FMQ, 
  fmq <- fmq_query_list %>%
    group_by(PT) %>%
    mutate(
      fmq_nam = paste0(paste0(FMQ, "/", FMQCAT), collapse = "~~"),
      PT = toupper(PT)
    ) %>%
    select(c(PT, fmq_nam)) %>%
    distinct_all()

  # smq <- read_sas("smq250.sas7bdat") %>%
  #   group_by(PT_NAME, SOC_NAME) %>%
  #   mutate(
  #     smq_nam = paste0(unique(paste0(trimws(str_replace(SMQ_NAME1, "\\(SMQ\\)", "")), "/", LLT_SCOPE)), collapse = "~~"),
  #     PT_NAME = toupper(PT_NAME),
  #     SOC_NAME = toupper(SOC_NAME)
  #   ) %>%
  #   select(c(smq_nam, PT_NAME, SOC_NAME)) %>%
  #   distinct_all()

  # merging FMQ and SMQ to AE ADaM Data
  dout <<- df1 %>%
    mutate(across(where(is.character), str_trim),
      SOC_NAME = toupper(AESOC),
      PT_NAME = toupper(AEDECOD)
    ) %>%
    # left_join(smq, by = c("SOC_NAME", "PT_NAME")) %>%
    left_join(fmq, by = c("PT_NAME" = "PT"))
  print('data processing success')
  return(dout)
  
}
