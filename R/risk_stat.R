options(warn = -1)

#' Function to calculate risk statistics for treatment pairs from AE pre-processed data
#'
#' @param datain Input dataset after pre_processing of AE data with analysis subset applied
#'
#' @param d_datain Input dataset after pre_processing of AE data for denominator
#'
#' @param summary_by Measure to construct the summary by. Values: 'Patients' or 'Events'
#'
#' @param eventVar Event Variable to review by. Example: "AEDECOD", "AEBODSYS"
#'
#' @param ctrlgrp Control treatment value
#'
#' @param trtgrp Treatment(s) to create pairs. Only 1 value for Volcano/table,
#'               can be multiple for Forest
#' @param statistics Statistic to be calculated. Values: 'Risk Ratio' or 'Risk Difference'
#'
#' @param alpha Alpha value to determine confidence interval for risk calculation. Default: 0.05
#'
#' @param cutoff Incidence Cutoff Value - consider only terms with incidence percentage > ui_cutoff
#'
#' @param sort_opt How to sort terms, only for table/forest plot.
#' Values: 'Ascending','Descending','Alphabetical'
#'
#' @param sort_var Metric to sort by. Values: 'Count','Percent','RiskValue'
#'
#' @return a dataset containing risk statistic calculations for given treatment pair(s)
#' @export
#'
#' @examples
#' library(cvars)
#'
#' risk <- risk_stat(
#'   datain = ae_pre$dsin,
#'   d_datain = ae_pre$dout,
#'   summary_by = "Patients",
#'   eventVar = "AEDECOD",
#'   ctrlgrp = "Placebo",
#'   trtgrp = "Xanomeline High Dose",
#'   statistics = "Risk Ratio",
#'   alpha = 0.05,
#'   cutoff = 2,
#'   sort_opt = "Ascending",
#'   sort_var = "Count"
#' )
#'
#' risk[25:35, ]
risk_stat <- function(datain,
                      d_datain,
                      summary_by,
                      eventVar,
                      ctrlgrp,
                      trtgrp,
                      statistics,
                      alpha,
                      cutoff,
                      sort_opt,
                      sort_var) {
  if (nrow(datain) == 0) {
    return(NULL)
  }

  if (is.null(ctrlgrp) || is.null(trtgrp)) {
    return(NULL)
  }
  trtgrp <- unlist(strsplit(trtgrp, "~~"))
  getstats <- data.frame()

  ## getting equivalent data variable for given summary by selection
  summ_var <- recode(summary_by,
    "Patients" = "USUBJID",
    "Events" = eventVar
  )

  ## creating a processing function for risk calculation
  risk_process <- function(count1, count2, denom1, denom2) {
    temp_N1 <- denom1 - count1
    temp_N2 <- denom2 - count2

    rmat <- matrix(c(temp_N1, temp_N2, count1, count2), nrow = 2)
    if (statistics == "Risk Difference") {
      rv <- riskdiff_wald(rmat, conf.level = 1 - alpha)
    } else if (statistics == "Risk Ratio") {
      rv <- riskratio.wald(rmat, conf.level = 1 - alpha)
    }
    rd <- round(rv$measure[2, 1], 3)
    rdp <- round(rv$p.value[2, 3], 4)
    rdcil <- round(rv$measure[2, 2], 4)
    rdciu <- round(rv$measure[2, 3], 4)

    output <- c(rd, rdp, rdcil, rdciu)
    return(output)
  }

  ## looping through all the treatment pairs
  for (t in trtgrp) {
    treatment1 <- ctrlgrp
    treatment2 <- t
    ### Filtering based on selected treatment groups -----------------------------
    data1 <- datain %>%
      filter(TRTVAR %in% c(treatment1, treatment2))

    ## calling Mcatstat for getting n count and %
    mcatstatd <- mcatstat(
      datain = data1,
      d_datain = d_datain,
      ui_uniqid = ifelse(summary_by == "Patients", summ_var, NA),
      ui_dptvar = eventVar,
      ui_pctdisp = "TRT"
    )

    ##  get Count1 and count2 of ctrl group aand trt group for risk calculation
    idvar <- c("BYVAR1", "DPTVAL")
    ## calculating the risk values
    mcatd <- mcatstatd %>%
      mutate(
        TRTCD = ifelse(TRTVAR == treatment1, "CTRLGRP", ifelse(TRTVAR == treatment2, "TRTGRP", "")),
        PCT = as.numeric(PCT)
      ) %>%
      tidyr::pivot_wider(
        id_cols = any_of(c(idvar)), names_from = TRTCD,
        values_from = c(FREQ, PCT, DENOMN)
      ) %>%
      group_by(across(all_of(c(idvar)))) %>%
      mutate_if(is.numeric, ~ tidyr::replace_na(., 0)) %>%
      mutate(result = paste(risk_process(FREQ_CTRLGRP, FREQ_TRTGRP, DENOMN_CTRLGRP, DENOMN_TRTGRP),
        collapse = " "
      )) %>%
      filter(PCT_CTRLGRP > cutoff | PCT_TRTGRP > cutoff) # filter based on cutoff value

    ## check for data after cut off
    if (nrow(mcatd) == 0) {
      return(NULL)
    }

    aeSummCutOff <- mcatd %>%
      mutate(
        PVALUE = round(as.numeric(unlist(strsplit(result, " "))[2]), 4),
        ADJPVALUE = p.adjust(unlist(strsplit(result, " "))[2], method = "fdr"),
        RISK_CI = paste0(
          round(as.numeric(unlist(strsplit(result, " "))[1]), 3),
          " (",
          round(as.numeric(unlist(strsplit(result, " "))[3]), 2),
          ",",
          round(as.numeric(unlist(strsplit(result, " "))[4]), 2),
          ")"
        ),
        RISK = round(as.numeric(unlist(strsplit(result, " "))[1]), 3),
        RISKCIL = round(as.numeric(unlist(strsplit(result, " "))[3]), 2),
        RISKCIU = round(as.numeric(unlist(strsplit(result, " "))[4]), 2),
        CTRL_N = FREQ_CTRLGRP, CTRL_PCT = PCT_CTRLGRP,
        TRTPAIR = paste0(treatment1, " -vs- ", treatment2)
      ) %>%
      select(-result) %>%
      tidyr::pivot_longer(
        cols = !(c(
          idvar, PVALUE, ADJPVALUE, RISK_CI, RISK, RISKCIL, RISKCIU,
          CTRL_N, CTRL_PCT, TRTPAIR
        )),
        names_to = "TRTVAR", values_to = "VAL"
      ) %>%
      tidyr::separate(col = TRTVAR, into = c("STAT", "TRTVAR"), sep = "_") %>%
      tidyr::pivot_wider(
        id_cols = c(
          idvar, PVALUE, ADJPVALUE, RISK_CI, RISK, RISKCIL, RISKCIU,
          CTRL_N, CTRL_PCT, TRTPAIR, TRTVAR
        ),
        names_from = STAT, values_from = VAL
      ) %>%
      mutate(
        TRTVAR = recode(TRTVAR, "CTRLGRP" = treatment1, "TRTGRP" = treatment2),
        TOTAL_N = DENOMN,
        HOVER_TEXT = paste0(
          "\n", eventVar, " = ", DPTVAL,
          "\n n of ", ifelse(summary_by == "Patients", "Participants", summary_by), "= ", FREQ,
          "\n", "Risk(CI) = ", RISK_CI,
          "\n p-value = ", PVALUE
        )
      )
    ## append all the treatment pair dataset to final data
    if (nrow(getstats) == 0) {
      getstats <- aeSummCutOff
    } else {
      getstats <- rbind(getstats, aeSummCutOff)
    }
  }

  ## Sorting the data based in the different sort options
  if (!is.na(sort_opt) && !is.na(sort_var)) {
    getstats <- getstats %>%
      ungroup() %>%
      {
        if (sort_opt == "Ascending") {
          if (sort_var == "Count") {
            arrange(., CTRL_N)
          } else if (sort_var == "Percent") {
            arrange(., CTRL_PCT)
          } else if (sort_var == "RiskValue") {
            arrange(., RISK)
          }
        } else if (sort_opt == "Descending") {
          if (sort_var == "Count") {
            arrange(., desc(CTRL_N))
          } else if (sort_var == "Percent") {
            arrange(., desc(CTRL_PCT))
          } else if (sort_var == "RiskValue") {
            arrange(., desc(RISK))
          }
        } else if (sort_opt == "Alphabetical") {
          arrange(., eventVar)
        } else {
          .
        }
      }
  }
  return(getstats)
}
