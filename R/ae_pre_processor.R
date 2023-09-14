#' Pre-processing data for Adverse Event domain specific reports
#'
#' @param datain the ADaM dataset read from the `dataread()` utility is taken as input data.
#' @param ae_filter The filter UI elements that are applied for the report is taken as input.
#' @param aeSubset Subset conditions for analysis of dependent variable.
#' @param aeDenomSubset Subset conditions for overall report
#' @param aeObsPeriod applied for considering AE event that happened in specific duration.
#' if user pass "Overall duration" it will filter the `STUDYFL == "Y"` records. "Other" will filter
#' the period which falls under the start and end date.
#' @param aeObsResidual If `aeObsResidual` sets to "Other", Use this argument to pass a period
#' (in numeric) to extend the obseravation period.
#' @param trtvar Treatment variable name string assigned as TRTVAR.
#' @param trtsort Treatment sorting variable - numeric or character
#' @param pop_fil Population Filter for data set.
#' @param fmq_data FMQ table data set
#' @param aeEventVar Event term variable - Lower Level
#' @param aeByVar By variable name string assigns to BYVAR1,BYVAR2, etc
#' @param aeSubGrpVar Subgroup variable name string assigns to SUBGRPVAR1, SUBGRPVAR2, etc.,
#' @param aeBigN Display big N in column header (y/n).
#' @param aeGrpVarMiss Include missing values of by group and sub group variables "Y"/"N"
#' @param aeTrtTot Display total treatment column in table or plot (y/n).
#' @param aeSubGrpTot Display total AE subgroup column in treatment or plot (y/n)
#'
#' @return : a list containing 3 objects
#'  \itemize{
#'  \item dsin - Processed dataframe output for further utilities, with analysis subset
#'  \item dout - Processed dataframe output to be carried to further utilities; no analysis subset
#'  \item trtbign - table or numeric value for big N count, pass to mdisplay
#'  }
#'
#' @export
#'
#' @examples
#' library(cvars)
#' data(adae)
#' data(FMQ_Consolidated_List)
#' data_pre <- ae_pre_processor(
#'   datain = adae,
#'   aeSubset = "AOCCPFL=='Y'",
#'   aeDenomSubset = "!is.na(ASTDT)",
#'   ae_filter = "Any Event",
#'   aeObsPeriod = "Overall Duration",
#'   aeObsResidual = 0,
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   pop_fil = "SAFFL",
#'   fmq_data = FMQ_Consolidated_List,
#'   aeEventVar = "AEDECOD",
#'   aeByVar = "AEBODSYS",
#'   aeSubGrpVar = NA,
#'   aeBigN = "Y",
#'   aeGrpVarMiss = "Y",
#'   aeTrtTot = "Y",
#'   aeSubGrpTot = "Y"
#' )
#'
#' data_pre$dout
ae_pre_processor <- function(
    datain,
    ae_filter = "Any Event",
    aeSubset = "AOCCPFL=='Y'",
    aeDenomSubset = "!is.na(ASTDT)",
    aeObsPeriod = "Overall Duration",
    aeObsResidual = 0,
    trtvar = "TRTA",
    trtsort = "TRTAN",
    pop_fil = "SAFFL",
    fmq_data = NA,
    aeEventVar = "AEDECOD",
    aeByVar = "AEBODSYS",
    aeSubGrpVar = NA,
    aeBigN = "Y",
    aeGrpVarMiss = "Y",
    aeTrtTot = "Y",
    aeSubGrpTot = "Y") {
  # Processing FMQ variables
  if ((aeEventVar == "FMQ_NAM" || (all(!is.na(aeByVar)) && aeByVar == "FMQ_NAM")) &&
    all(!is.na(fmq_data))) {
    # Reading FMQ,
    fmq <- fmq_data %>%
      group_by(PT) %>%
      mutate(
        FMQ_NAM = paste0(paste0(FMQ, "/", FMQCAT), collapse = "~~"),
        PT = toupper(PT)
      ) %>%
      select(c(PT, FMQ_NAM)) %>%
      distinct_all()

    # merging FMQ to AE ADaM Data
    datain <- datain %>%
      mutate(across(where(is.character), str_trim),
        SOC_NAME = toupper(AESOC),
        PT_NAME = toupper(AEDECOD)
      ) %>%
      left_join(fmq, by = c("PT_NAME" = "PT"))
  }


  # pre process for AE data

  # standardizing date format to common format
  date_formats <- c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  if ("AESTDT" %in% names(datain)) {
    data_pro <- datain %>%
      mutate(AESTDT = as.Date(AESTDT, tryFormats = date_formats, optional = FALSE))
  } else {
    data_pro <- datain %>%
      mutate(AESTDT = as.Date(ASTDT, tryFormats = date_formats, optional = FALSE))
  }
  if ("AEENDT" %in% names(datain)) {
    data_pro <- data_pro %>%
      mutate(AEENDT = as.Date(AEENDT, tryFormats = date_formats, optional = FALSE))
  } else {
    data_pro <- data_pro %>%
      mutate(AEENDT = as.Date(AENDT, tryFormats = date_formats, optional = FALSE))
  }
  if ("RFSTDTC" %in% names(datain)) {
    data_pro <- data_pro %>%
      mutate(RFSTDTC = as.Date(RFSTDTC, tryFormats = date_formats, optional = FALSE))
  } else {
    data_pro <- data_pro %>%
      mutate(RFSTDTC = as.Date(TRTSDT, tryFormats = date_formats, optional = FALSE))
  }

  if ("RFENDTC" %in% names(datain)) {
    data_pro <- data_pro %>%
      mutate(RFENDTC = as.Date(RFENDTC, tryFormats = date_formats, optional = FALSE))
  } else {
    data_pro <- data_pro %>%
      mutate(RFENDTC = as.Date(TRTEDT, tryFormats = date_formats, optional = FALSE))
  }

  # Eliminating not coded AE terms and recodeing Toxicity to severity
  data_pro <- data_pro %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = if_else(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = if_else(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT)
    )
  if ("AESEV" %in% (names(data_pro))) {
    data_pro <- data_pro %>% mutate(AESEV = toupper(AESEV))
  } else if ("ASEV" %in% names(data_pro)) {
    data_pro <- data_pro %>% mutate(AESEV = toupper(ASEV))
  } else if ("ATOXGR" %in% (names(data_pro))) {
    data_pro <- data_pro %>% mutate(AESEV = recode(toupper(ATOXGR),
      "GRADE 0" = "MILD",
      "GRADE 1" = "MILD",
      "GRADE 2" = "MODERATE",
      "GRADE 3" = "MODERATE",
      "GRADE 4" = "SEVERE",
      "GRADE 5" = "SEVERE"
    ))
  }
  ## Calling Mentry to process treatment variable , grouping variables, total trt, Big N calculation
  mdsin <- mentry(
    datain = data_pro,
    ui_aSubset = aeSubset,
    ui_dSubset = aeDenomSubset,
    ui_byvar = aeByVar,
    ui_subgrpvar = aeSubGrpVar,
    ui_trtvar = trtvar,
    ui_trtsort = trtsort,
    ui_trttotalyn = aeTrtTot,
    ui_sgtotalyn = aeSubGrpTot,
    ui_bign = aeBigN,
    ui_addGrpMiss = aeGrpVarMiss,
    ui_pop_fil = pop_fil
  )
  dsin <- mdsin$dsin
  # applying fliter conditional from the UI filter element for AE reports
  # filter data for seriousness, drug-related, and severity
  if (length(ae_filter) > 0) {
    if ("Any Event" %in% ae_filter) {
      dsin <- dsin
    }
    if ("Treatment emergent" %in% ae_filter) {
      dsin <- dsin %>% filter(TRTEMFL == "Y")
    }
    if ("Serious" %in% ae_filter) {
      dsin <- dsin %>% filter(AESER == "Y")
    }

    if ("Drug-related" %in% ae_filter) {
      dsin <- dsin %>% filter(AEREL == "RELATED")
    }
    if (sum(c("Mild", "Moderate", "Severe") %in% ae_filter) > 0) {
      severity_filter <- ae_filter[which(ae_filter %in% c("Mild", "Moderate", "Severe"))]
      dsin <- dsin %>% filter(AESEV %in% toupper(severity_filter))
    }

    if (sum(c(
      "Recovered/Resolved", "Recovering/Resolving",
      "Not Recovered/Not Resolved", "Fatal"
    ) %in% ae_filter) > 0) {
      severity_filter <- ae_filter[which(ae_filter %in% c(
        "Recovered/Resolved",
        "Recovering/Resolving",
        "Not Recovered/Not Resolved", "Fatal"
      ))]
      dsin <- dsin %>% filter(AEOUT %in% toupper(severity_filter))
    }
  }

  # filter ae data occurred in the given time frame
  if (aeObsPeriod == "Overall Duration") {
    if ("STUDYFL" %in% names(dsin)) {
      dsin <- dsin %>%
        filter(STUDYFL == "Y")
    } else {
      dsin <- dsin
    }
  } else if (aeObsPeriod == "Other") {
    dsin <- dsin %>%
      filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + aeObsResidual)))
  }
  return(list(dsin = dsin, dout = mdsin$dout, bigN = mdsin$bign))
}
