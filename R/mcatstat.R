#' Summary analysis for categorical variables
#' \code{mcatstat()} returns a new dataframe containing counts and percentages
#' of a categorical analysis variable according to grouping and treatment
#' variables passed in \code{mentry()}
#'
#' @param datain Input data from `mentry()` output (dsin) to get counts for each
#'  category
#' @param d_datain Input data from `mentry()` output (dout) to calculate the
#' denominator for percentage calculation
#' of the counts from `datain`
#' @param ui_uniqid Variable to calculate unique counts of.
#' Likely values: "USUBJID", "SITEID", NA
#' @param ui_dptvar Categorical Analysis variable. eg: "SEX","AEDECOD"
#' @param ui_pctdisp Method to calculate denominator (for %) by.
#' Possible values: "TRT","VAR","COL","SUBGRP","CAT","NONE","NO","DPTVAR"
#' @param miss_catyn To include empty/blank values as "Missing" in categories of
#' `ui_dptvar` variable or not. Values: "Y"/"N"
#' @param cum_ctyn To return cumulative frequency instead of individual
#' frequencies for each category. Possible values: "Y"/"N"
#' @param total_catyn To return a 'Total' row for the categories of `ui_dptvar`
#' variable or not. Possible values: "Y"/"N"
#' @param dptvarn Number to assign as 'DPTVARN', useful for block sorting when
#' multiple `mcatstat()` outputs are created to be combined.
#'
#' @details
#' \itemize{
#' \item Objects passed to `datain` and `d_datain` can be the $dsin and $dout
#' return elements respectively from either \code{mentry()} or
#' \code{ae_pre_processor()}
#' \item `ui_uniqid` is the variable name to get unique counts of. If given as
#' NA, it sums all observations for the given category. If "USUBJID" then it
#' calculates the number of unique subjects per category.
#' \item `cum_ctyn` as "Y" to get output value as cumulative frequencies instead
#'  of individual frequencies. If "Y", `total_catyn` will be reset to "N"
#' \item pctdisp has possible values for method to get denominator to calculate
#'  percentage:
#'       NONE/NO: No percent calculation
#'       TRT: Treatment total counts acts as denominator
#'       VAR: Variable Total of all treatments/groups acts as denominator
#'       COL: Column wise denominator - percentage within each
#'        Treatment-Subgroup(s) combination
#'       CAT: Row-wise denominator - percentage within each Bygroup(s)-dptvar
#'        combination
#'       SUBGRP: Percentage within each Treatment-By group(s)-Subgroup(s)
#'        combination
#'       DPTVAR: Percentage within each Treatment-By group(s)-Subgroup(s)-dptvar
#'        combination.
#' }
#'
#' @return a data.frame with counts and/or percentages, passed to
#'  \code{mdisplay()} or for further risk statistic calculation
#' @export
#'
#' @examples
#' data(ae_pre)
#'
#' ae_cat <- mcatstat(
#'   datain = ae_pre$dsin,
#'   d_datain = ae_pre$dout,
#'   ui_uniqid = "USUBJID",
#'   ui_dptvar = "AEDECOD",
#'   ui_pctdisp = "TRT"
#' )
#' ae_cat
mcatstat <- function(datain = NULL,
                     d_datain = NULL,
                     ui_uniqid = "USUBJID",
                     ui_dptvar = NULL,
                     ui_pctdisp = "TRT",
                     miss_catyn = "N",
                     cum_ctyn = "N",
                     total_catyn = "N",
                     dptvarn = 1) {
  if (nrow(datain) == 0) {
    print("Mcatstat has no data to display")
    return(NULL)
  }

  # Identify by groups if exists
  BYVAR <- var_start(datain, "BYVAR")
  # Identify subgroups if exists
  SUBGRP <- var_start(datain, "SUBGRP")
  BYVARN <- var_start(datain, "BYVARN")
  SUBGRPN <- var_start(datain, "SUBGRPN")

  # Data ungroup prior to counting
  datain <- datain %>% ungroup()
  d_datain <- d_datain %>% ungroup()
  # Set unique ID variable to get counts of
  if (!is.na(ui_uniqid)) {
    datain$UNIQID <- datain[[ui_uniqid]]
    d_datain$UNIQID <- d_datain[[ui_uniqid]]
  } else {
    datain <- datain %>% mutate(UNIQID = row_number())
    d_datain <- d_datain %>% mutate(UNIQID = row_number())
  }

  # Setting Categories
  datain$DPTVAL <- datain[[ui_dptvar]]
  d_datain$DPTVAL <- d_datain[[ui_dptvar]]

  # Convert empty categories to "Missing":
  if (is.character(datain$DPTVAL)) {
    datain <- datain %>% mutate(DPTVAL = ifelse(DPTVAL %in% c("", " ", NA),
      "Missing", DPTVAL
    ))
    d_datain <- d_datain %>% mutate(DPTVAL = ifelse(DPTVAL %in% c("", " ", NA),
      "Missing", DPTVAL
    ))
    # To display missing categories or not per miss_catyn
    if (miss_catyn == "N") {
      datain <- datain %>% filter(DPTVAL != "Missing")
    }
  }
  # Check if DPTVAL-N variable already exists for sorting purpose
  datain <- datain %>%
    rename(any_of(c("DPTVALN" = paste0(ui_dptvar, "N"))))

  # If DPTVAL-N does not exist already, create it
  if (!("DPTVALN" %in% names(datain))) {
    datain <- datain %>%
      arrange(DPTVAL) %>%
      mutate(DPTVALN = ifelse(as.character(DPTVAL) == "Missing", 999,
        as.integer(fct_inorder(as.character(DPTVAL)))
      ))
  }



  # Display a Total row as sum of all categories if total_catyn is Y
  if ((total_catyn == "Y") && (cum_ctyn != "Y")) {
    datain <- datain %>%
      mutate(DPTVAL = "Total", DPTVALN = 1000) %>%
      bind_rows(datain, .)
    d_datain <- d_datain %>%
      mutate(DPTVAL = "Total", DPTVALN = 1000) %>%
      bind_rows(datain, .)
  }


  # Set groups by Treatment, Sub,By if any to use for counts
  countgrp <- c("TRTVAR", SUBGRP, SUBGRPN, BYVAR, BYVARN, "DPTVAL", "DPTVALN")

  # Get N count as variable FREQ:
  counts <- datain %>%
    group_by(across(any_of(countgrp))) %>%
    summarise(FREQ = length(unique(UNIQID))) %>%
    ungroup()

  # If cumulative count is required then
  if (cum_ctyn == "Y") {
    counts <- counts %>%
      group_by(across(any_of(c("TRTVAR", BYVAR, SUBGRP)))) %>%
      arrange(DPTVALN, .by_group = TRUE) %>%
      mutate(FREQ = cumsum(FREQ)) %>%
      ungroup()
  }
  # Check Allowable pctdisp values
  validpct <- c(
    "TRT",
    "VAR",
    "COL",
    "SUBGRP",
    "SGRPN",
    "CAT",
    "NONE",
    "NO",
    "DPTVAR"
  )

  if (!((ui_pctdisp %in% validpct) || str_detect(ui_pctdisp, "BYVAR[0-9]+N"))) {
    return(NULL)
  }

  # Set denominator values for percentage
  if (ui_pctdisp %in% c("NONE", "NO")) {
    df <- counts %>% mutate(CVALUE = FREQ) # No percentage if pctdisp is NO/NONE
  } else {
    # Identify which variables go towards creating combo for Denominator
    if (ui_pctdisp == "VAR") {
      # If pctdisp = VAR, total percent across all records
      denom <-
        counts %>%
        ungroup() %>%
        mutate(DENOMN = length(unique(d_datain$UNIQID)))
    } else {
      if (ui_pctdisp == "TRT") { # Percent By each treatment if pctdisp = TRT
        percgrp <- "TRTVAR"
      } else if (ui_pctdisp == "CAT") { # Percent row-wise if pctdisp=CAT
        percgrp <- c(BYVAR, "DPTVAL")
      } else if (ui_pctdisp == "COL") { # Percent column-wise if pctdisp=COL
        percgrp <- c("TRTVAR", SUBGRP)
      } else if (ui_pctdisp == "SUBGRP") { # Percent by Treatment, By and
        # Subgroup if pctdisp=SUBGRP
        percgrp <- c("TRTVAR", SUBGRP, BYVAR)
      } else if (ui_pctdisp == "SGRPN") { # Percent if pctdisp=SUBGRP
        percgrp <- SUBGRP
      } else if (str_detect(ui_pctdisp, "BYVAR[0-9]+N")) { # Percent by
        # combination of By Variables if pctdisp = BYVARXYN
        bys <-
          paste0("BYVAR", as.integer(unlist(str_split(
            str_extract(ui_pctdisp, "[[:digit:]]+"), ""
          ))))
        percgrp <- c("TRTVAR", bys)
      } else if (ui_pctdisp == "DPTVAR") { # row-wise calculation with By, Trt
        # and Sub. Almost always 100%
        percgrp <- c("TRTVAR", SUBGRP, BYVAR, "DPTVAL")
      }
      # Take denominator variables only when they exist in dataframe
      percgrp <- percgrp[percgrp %in% names(datain)]

      # Get denominator count per above variables
      denom <- d_datain %>%
        group_by(across(all_of(percgrp))) %>%
        summarise(DENOMN = length(unique(UNIQID))) %>%
        inner_join(counts, by = percgrp, multiple = "all")
    }

    # Calculate percentage as PCT and concatenate as CVALUE
    df <- denom %>%
      mutate(
        PCT = format(round((FREQ * 100) / DENOMN, 2), nsmall = 2),
        CVALUE = paste0(FREQ, " (", PCT, "%)")
      )
  }

  # Add requisite variables for standard mdisplay processing:
  df <- df %>%
    ungroup() %>%
    mutate(
      DPTVAR = ui_dptvar,
      XVAR = DPTVAL,
      DPTVARN = dptvarn, CN = "C"
    ) %>%
    select(DPTVAR, DPTVAL, XVAR, everything())

  print("mcat success")

  return(df)
}
