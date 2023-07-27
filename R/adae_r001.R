#' Adverse Events Summary Table
#'
#' @param datain List containing input datasets, from `ae_pre_processor()`
#' @param population Population filter applied, for the footnote.
#' @param AE_Filter Adverse Event Filter applied, for the footnote
#' @param riskyn To calculate and display Risk Statistic or not. ("Y"/"N")
#' @param summary_by Measure to construct the summary by. Values: "Patients", "Events"
#' @param ctrlgrp Treatment Variable value which becomes control group
#' @param trtgrp Treatment Variable value which becomes treatment group
#' @param ui_lt Low Level Adverse Event term variable, used for analysis
#' @param ui_ht High Level Adverse Event term variable, used for analysis
#' @param ui_statistics Measure to be calculated. Values: "Risk Difference", "Risk Ratio"
#' @param ui_trttotalyn To display Total Column for Treatments or not. Values: "Y"/"N"
#' @param ui_pctdisp Method to set denominator for percentage incidence.
#' Values: "TRT", "VAR", "HT"
#' @param ui_alpha Alpha value to determine confidence interval for risk calculation
#' @param ui_cutoff Incidence Cutoff Value - subset only Higher Level terms with incidence
#' percentage > ui_cutoff value. All corresponding Low terms are taken.
#' @param ui_sortopt How to sort terms in output. Values: "Ascending", "Descending", "Alphabetical"
#' @param ui_sortvar Metric to sort terms by. Values: "Count", "Percent", "RiskValue"
#'
#' @details
#' \itemize{
#' \item Object passed to `datain` must be the output of \code{ae_pre_processor},
#' containing elements `dsin`, `dout` and `bigN`. Element `bigN` determines whether the
#' output table contains (N = count) appended to each Treatment Column. Elements
#' `dsin` and `dout` are used to calculate counts and percentages respectively.
#' \item Arguments `population` and `AE_Filter` are used only in report title and footnote.
#' They should be the same values passed previously in the `ae_pre_processor()` function.
#' \item If `summary_by` is "Patients", then the count used is the unique number of participants
#' per term; and if it is "Events", the count used is the total number of events observed per term.
#' \item Arguments `ui_lt` and `ui_ht` contain the lower and higher level Event variables.
#' The lower level variable will be grouped by higher level, so apt variables must be passed.
#' Examples (Low, High): AEDECOD/AEBODSYS, FMQ_NAM/AEBODSYS or AEDECOD/FMQ_NAM.
#' Passing AEBODSYS/AEDECOD or AEBODSYS/FMQ_NAM may not give expected results.
#' \item Passing "Y" to `riskyn` argument:
#' Includes pair-wise risk statistic given by `ui_statistics` (Risk Difference or Risk Ratio).
#' Only the treatments passed in `ctrlgrp` and `trtgrp` will be considered as the pair.
#' `ui_pctdisp` is automatically set to "TRT" (Risk/percent is calculated for each treatment)
#' \item Passing "N" to `riskyn` argument:
#' No risk statistic, only incidence counts and percentage are displayed for all treatments.
#' Arguments `ctrlgrp`, `trtgrp`, `ui_statistics` and `ui_alpha` are not required
#' Only "Count" and "Percent" are permitted for `ui_sortvar`.
#' `ui_pctdisp` determines the denominator to calculate %. It can be "TRT"
#' (Denominator is Treatment total counts), "VAR" (Variable Total of all treatments/groups)
#' or "HT" (High Term uses Treatment counts and Low Term uses High Term counts per treatment)
#' \item `ui_sortopt` and `ui_sortvar` act together. For example, giving "Ascending" and
#' "Count" will sort the terms in ascending order of counts in the table.
#' If `ui_sortopt` is "Alphabetical", `ui_sortvar` is not considered.
#' }
#'
#' @return a list containing 4 objects
#' \itemize{
#' \item tout - Flextable output for display
#' \item rpt_data - Final data.frame used to create `tout`
#' \item title - Report title
#' \item footnote - Report Footnote
#' }
#' @export
#'
#' @examples
#' library(cvars)
#' data(ae_pre)
#' ae_out <- adae_r001(
#'   datain = ae_pre,
#'   population = "Overall",
#'   AE_Filter = "",
#'   riskyn = "Y",
#'   ctrlgrp = "Placebo",
#'   trtgrp = "Xanomeline Low Dose",
#'   ui_statistics = "Risk Ratio",
#'   ui_lt = "AEDECOD",
#'   ui_ht = "AEBODSYS",
#'   ui_cutoff = 5
#' )
#'
#' ae_out$tout
#'
adae_r001 <- function(datain,
                      population = "Overall",
                      AE_Filter = "",
                      riskyn = "N",
                      summary_by = "Patients",
                      ctrlgrp,
                      trtgrp,
                      ui_lt = "AEDECOD",
                      ui_ht = "AEBODSYS",
                      ui_statistics = "Risk Difference",
                      ui_trttotalyn = "N",
                      ui_pctdisp = "TRT",
                      ui_alpha = 0.05,
                      ui_cutoff = 5,
                      ui_sortopt = "Ascending",
                      ui_sortvar = "Count") {
  if (riskyn == "N") {
    a_data <- datain$dsin %>% mutate(BYVAR1 = eval(parse(text = ui_ht)))
    d_data <- datain$dout %>% mutate(BYVAR1 = eval(parse(text = ui_ht)))

    # PCTDISP translates to BYVAR1N for LT and TRT for HT if SOC is given as ui_pctdisp
    summ_id <- ifelse(summary_by == "Events", NA, "USUBJID")
    ## Mcatstat for Low Term
    lt_dat <- mcatstat(
      datain = a_data,
      d_datain = d_data,
      ui_uniqid = summ_id,
      ui_dptvar = ui_lt,
      ui_pctdisp = recode(ui_pctdisp, "HT" = "BYVAR1N")
    ) %>% ungroup()


    ## mcatstat for High Term

    ht_dat <- mcatstat(
      datain = a_data,
      d_datain = d_data,
      ui_uniqid = summ_id,
      ui_dptvar = ui_ht,
      ui_pctdisp = recode(ui_pctdisp, "HT" = "TRT")
    ) %>% ungroup()

    # Cut off - apply to high term only:
    ht_dat <-
      subset(ht_dat, BYVAR1 %in% unique(BYVAR1[as.numeric(PCT) > ui_cutoff]))

    # Vector of Unique High terms
    uniqHT1 <- unique(ht_dat[ht_dat$TRTVAR == levels(ht_dat$TRTVAR)[1], ]$DPTVAL)
    uniqHT <- c(
      uniqHT1,
      unique(ht_dat$DPTVAL)[!(unique(ht_dat$DPTVAL) %in% uniqHT1)]
    )

    if (!is.na(ui_sortopt) && !is.na(ui_sortvar)) {
      if (ui_sortopt == "Alphabetical") {
        ht_dat <- ht_dat %>% arrange(DPTVAL)
        lt_dat <- lt_dat %>% arrange(BYVAR1, DPTVAL)
        uniqHT <- unique(ht_dat$DPTVAL)
      } else {
        lt_dat <- lt_dat %>% mutate(
          CTRL_F = ifelse(TRTVAR == levels(TRTVAR)[1], FREQ, NA),
          CTRL_P = ifelse(TRTVAR == levels(TRTVAR)[1], PCT, NA)
        )
        ht_dat <- ht_dat %>% mutate(
          CTRL_F = ifelse(TRTVAR == levels(TRTVAR)[1], FREQ, NA),
          CTRL_P = ifelse(TRTVAR == levels(TRTVAR)[1], PCT, NA)
        )
        if (ui_sortopt == "Ascending") {
          if (ui_sortvar == "Count") {
            ht_dat <- ht_dat %>% arrange(CTRL_F)
            lt_dat <- lt_dat %>% arrange(CTRL_F)
          } else {
            ht_dat <- ht_dat %>% arrange(CTRL_P)
            lt_dat <- lt_dat %>% arrange(CTRL_P)
          }
        } else if (ui_sortopt == "Descending") {
          if (ui_sortvar == "Count") {
            ht_dat <- ht_dat %>% arrange(desc(CTRL_F))
            lt_dat <- lt_dat %>% arrange(desc(CTRL_F))
          } else {
            ht_dat <- ht_dat %>% arrange(desc(CTRL_P))
            lt_dat <- lt_dat %>% arrange(desc(CTRL_P))
          }
        }
        uniqHT1 <- ht_dat %>%
          filter(TRTVAR == levels(TRTVAR)[1]) %>%
          unique()
        uniqHT <- c(uniqHT1, unique(ht_dat$DPTVAL)[!(unique(ht_dat$DPTVAL) %in% uniqHT1)])
      }
    } else {
      ht_dat <- ht_dat %>% arrange(BYVAR1N)
      uniqHT <- unique(ht_dat$DPTVAL)
    }
  } else if (riskyn == "Y") {
    # Risk and counts for high term
    ht_dat <- risk_stat(
      datain = datain$dsin,
      d_datain = datain$dout,
      summary_by = summary_by,
      eventVar = ui_ht,
      ctrlgrp = ctrlgrp,
      trtgrp = trtgrp,
      statistics = ui_statistics,
      alpha = ui_alpha,
      cutoff = ui_cutoff,
      sort_opt = ui_sortopt,
      sort_var = ui_sortvar
    )

    # Risk and counts for Low Term
    lt_dat <- risk_stat(
      datain = datain$dsin,
      d_datain = datain$dout,
      summary_by = summary_by,
      eventVar = ui_lt,
      ctrlgrp = ctrlgrp,
      trtgrp = trtgrp,
      statistics = ui_statistics,
      alpha = ui_alpha,
      cutoff = 0,
      sort_opt = ui_sortopt,
      sort_var = ui_sortvar
    ) %>% mutate(DPTVAR = ui_lt, CN = "C")

    if (is.null(ht_dat)) {
      final_cts <- data.frame("Note" = "No data available under these conditions")
      return(list(
        tout = flextable(final_cts),
        rpt_data = final_cts
      ))
    } else {
      ht_dat <- ht_dat %>% mutate(DPTVAR = ui_ht, CN = "C")
    }

    uniqHT1 <- unique(ht_dat[ht_dat$TRTVAR == ctrlgrp, ]$DPTVAL)
    uniqHT <- c(
      uniqHT1,
      unique(ht_dat$DPTVAL)[!(unique(ht_dat$DPTVAL) %in% uniqHT1)]
    )
  }



  # Common sorting for both Risk/No risk cases:
  # Block sorting identifier DPTVARN
  ht_dati <- ht_dat %>%
    select(BYVAR1, DPTVAL) %>%
    distinct() %>%
    mutate(DPTVARN = with(., match(DPTVAL, uniqHT))) %>%
    mutate(DPTVALN = 0) # Within block sorting DPTVALN - first row of every block

  # Match block sorting from high term data
  lt_dati <- lt_dat %>%
    select(BYVAR1, DPTVAL) %>%
    distinct() %>%
    mutate(DPTVARN = with(., match(BYVAR1, uniqHT))) %>%
    filter(!is.na(DPTVARN)) %>%
    # Within block sorting DPTVALN - retain order of getstats or mcatstat
    group_by(DPTVARN) %>%
    mutate(DPTVALN = row_number()) %>%
    ungroup()



  # Order of terms only:
  comb_order <- rbind(ht_dati, lt_dati)


  # Merge to get correct order:
  final_cts <- bind_rows(ht_dat, lt_dat) %>%
    select(-any_of(c("DPTVARN", "DPTVALN"))) %>%
    inner_join(comb_order, by = c("BYVAR1", "DPTVAL")) %>%
    select(-any_of(c("BYVAR1", "BYVAR1N")))

  # Report title and footnote
  if (is.null(AE_Filter)) {
    AE_Filter <- ""
  }
  AE_cond <- ifelse(length(AE_Filter) <= 1,
    sub(" Event", "", AE_Filter),
    paste0(
      paste0(AE_Filter[-length(AE_Filter)],
        collapse = ", "
      ),
      " and ",
      AE_Filter[length(AE_Filter)]
    )
  )

  title <-
    paste0(
      "Participants With ",
      AE_cond,
      " Adverse Events by Higher Term and Lower Term \n",
      population,
      " population"
    )
  footnote <-
    paste0(
      "* n is the number of participants with ",
      AE_cond,
      " adverse events."
    )

  # Final Processing for Risk Data
  if (riskyn == "Y") {
    extra_var <- paste0(ui_statistics, " (CI)")
    # Rename variable containing RISK_CI using ui_statistics input
    final_cts <- final_cts %>%
      mutate(RISK_CI = str_replace_all(RISK_CI, c(
        "NaN" = "NE",
        "Inf" = "NE",
        "-Inf" = "NE"
      )), RISK_CI = ifelse(startsWith(RISK_CI, "NE"), "NE", RISK_CI)) %>%
      rename(!!extra_var := "RISK_CI")

    # If CVALUE not present from risk_stat, create it:
    if (!("CVALUE" %in% names(final_cts))) {
      final_cts <-
        final_cts %>% mutate(CVALUE = paste0(FREQ, " (", PCT, "%)"))
    }

    # Additional Footnote
    footnote <-
      paste0(
        footnote,
        "\n",
        ui_statistics,
        " is shown between ",
        ctrlgrp,
        " and ",
        trtgrp
      )
  } else {
    extra_var <- ""
  }

  ### Mdisplay:
  report1 <- mdisplay(
    datain = final_cts,
    ui_bylabel = NA,
    ui_catlabel = ", ",
    ui_statlabel = NA,
    extra_vars = extra_var,
    trtbign = datain$bigN,
    colformat = " n(%)"
  )


  print("adae table sucess")
  return(list(
    tout = report1$tout,
    rpt_data = final_cts,
    title = title,
    footnote = footnote
  ))
}
