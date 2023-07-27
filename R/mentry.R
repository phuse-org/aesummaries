#' Function to read in and process data with subsets and variables.
#'
#' @description
#'
#' It takes arguments for TRT,BY and/or SUBGROUP variables, subset conditions and processes input
#' data. This data flows into categorical and numerical utilities
#'
#' @param datain Input dataset - can be any ADaM data
#' @param ui_aSubset Subset conditions for analysis of dependent variable
#' @param ui_dSubset Subset conditions for overall report
#' @param ui_byvar By variable name string assigns to BYVAR1,BYVAR2, etc
#' @param ui_subgrpvar Subgroup variable name string assigns to SUBGRPVAR1, SUBGRPVAR2, etc
#' @param ui_trtvar Treatment variable name string assigned as TRTVAR
#' @param ui_trtsort Treatment sorting variable - numeric or character
#' @param ui_trttotalyn Display total treatment column in table or plot (y/n)
#' @param ui_sgtotalyn Display total subgroup column in treatment or plot (y/n)
#' @param ui_bign Display big N in column header (y/n)
#' @param ui_addGrpMiss Add row or column for missing category in grouping variables
#' @param ui_pop_fil Population Filter for dataset
#'
#' @details
#'    1. By Variabel Processing
#'        Identifying byvars if byvar is given, create BYVAR1,2,etc
#'        If byvars also have a corresponding numeric code list variable then create BYVAR1N..
#'        for sorting purpose check for numeric code list variable for the subgroup var,
#'        if not create numeric variable
#'        Convert to character if numeric
#'        Re-code blanks from Bygrp vars as Missing
#'        remove missing category value from grouping variable as per user choice
#'    2. Treatment Variable Processing
#'        Identifying Treatment Variable,treatment sorting variable
#'        removed non treatment values
#'        If treatment sort variable doesn't exist, make it same as TRTVAR
#'        If treat sort is not numeric, convert to numeric
#'        treatment total column
#'        Convert TRTVAR to a factor in same order as TRTSORT
#'    3. Subgroup Variable processing
#'        Identifying Subgroup variable
#'        check for numeric code list variable for the subgroup var, if not create numeric variable
#'        Re-code blanks from By and Subgroup vars as Missing
#'        remove missing category value from grouping variable as per user choice
#'        Subgroup total column for the last subgroup variable
#'    4. Subset Processing
#'        applying population subset selected
#'        applying denominator/over all subset condition passed by the user
#'        applying analysis subset condition passed by the user
#'    5. Calculate big N for mdisplay
#'        big n if treatment exist or subgroup exists
#'        big n if both treatment & Subgroup don't exist
#'        If no big N required then NA
#'
#' @return a list containing 3 objects
#' \itemize{
#' \item `dsin`- processed dataframe output to be carried to further utilities
#' \item `bign`- table or numeric value for big N count, pass to mdisplay
#' \item `dout`- Subset data used for denominator calculation in categorical analysis
#' }
#' @export
#'
#' @examples
#' library(cvars)
#' data(adsl)
#' datain <- mentry(
#'   datain = adsl,
#'   ui_aSubset = "EFFFL=='Y'",
#'   ui_dSubset = NA,
#'   ui_byvar = "SEX,AGEGR1",
#'   ui_trtvar = "TRT01A",
#'   ui_trtsort = "TRT01AN",
#'   ui_subgrpvar = "SITEGR1,BMIBLGR1",
#'   ui_bign = "Y",
#'   ui_trttotalyn = "N",
#'   ui_addGrpMiss = "N",
#'   ui_sgtotalyn = "N",
#'   ui_pop_fil = "Overall Population"
#' )
#'
#' datain$dsin
#' datain$dout
#' datain$bign
#'
mentry <- function(datain,
                   ui_aSubset,
                   ui_dSubset,
                   ui_byvar = NA,
                   ui_subgrpvar = NA,
                   ui_trtvar = NA,
                   ui_trtsort = NA,
                   ui_trttotalyn = "N",
                   ui_sgtotalyn = "N",
                   ui_bign = "Y",
                   ui_addGrpMiss,
                   ui_pop_fil) {
  dsin <- datain
  # identifying byvars if byvar is given, create BYVAR1,2,etc
  if (!is.na(ui_byvar) && ui_byvar != "") {
    byvar <- unlist(strsplit(ui_byvar, ","))
    for (b in seq_along(byvar)) {
      dsin[[paste0("BYVAR", b)]] <- dsin[[byvar[b]]]
      # If byvars also have a corresponding numeric codelist vaiable then create BYVAR1N..
      # for sorting purpose check for numeric codelist variable for the subgroup var,
      # if not create numeric variable
      if (toupper(paste0(byvar[b], "N")) %in% names(dsin)) {
        dsin[[paste0("BYVAR", b, "N")]] <- dsin[[paste0(byvar[b], "N")]]
      } else {
        dsin[[paste0("BYVAR", b, "N")]] <-
          match(dsin[[paste0("BYVAR", b)]], unique(dsin[[paste0("BYVAR", b)]]))
      }
      dsin[[paste0("BYVAR", b)]] <- as.character(dsin[[paste0("BYVAR", b)]])
    }

    # Recode blanks from Bygrp vars as Missing
    dsin <- dsin %>%
      mutate(
        across(any_of(c(names(dsin)[startsWith(names(dsin), "BYVAR")])), ~ if_else(
          . %in% c("", " ", NA, NA_real_),
          if (is.character(.)) "Missing" else 998,
          .
        ))
      )

    # remove missing category value from grouping variable as per user choice
    if (ui_addGrpMiss == "N") {
      dsin <- dsin %>% filter(if_any(
        any_of(c(var_start(dsin, "BYVAR"))),
        function(x) !(x %in% c("", NA, "Missing"))
      ))
    }
  }

  # Identifying Treatment Variable
  if (!is.na(ui_trtvar) && ui_trtvar != "") {
    dsin[["TRTVAR"]] <- dsin[[ui_trtvar]]
    if (!is.na(ui_trtsort)) {
      dsin$TRTSORT <- dsin[[ui_trtsort]]
    }
  }

  if ("TRTVAR" %in% names(dsin)) {
    dsin$TRTVAR <- as.character(dsin$TRTVAR)
    # removed non treatment values
    dsin <- dsin %>% filter(!(toupper(TRTVAR) %in% c(
      "NOT ASSIGNED", "SCREEN FAILURE", "",
      NA, "SCRNFAIL", "NOTRT", "NOTASSGN"
    )))
    # If treatment sort variable doesnt exist, make it same as TRTVAR
    if (!("TRTSORT" %in% names(dsin))) {
      dsin$TRTSORT <- dsin$TRTVAR
    }
    # If treat sort is not numeric, convert to numeric
    if (!is.numeric(dsin$TRTSORT)) {
      dsin <- dsin %>% mutate(TRTSORT = match(TRTSORT, unique(TRTSORT)))
    }
    # treatment total column
    if (ui_trttotalyn == "Y") {
      dsin <- dsin %>% bind_rows(mutate(dsin, TRTVAR = "Total", TRTSORT = 999))
    }
    # Convert TRTVAR to a factor in same order as TRTSORT
    dsin$TRTVAR <- factor(dsin$TRTVAR,
      levels = unique(dsin$TRTVAR[order(dsin$TRTSORT)]),
      ordered = TRUE
    )
  }

  # Identifying Subgroup variabel
  if (!is.na(ui_subgrpvar) && ui_subgrpvar != "") {
    subgrpvar <- unlist(strsplit(ui_subgrpvar, ","))
    for (s in seq_along(subgrpvar)) {
      dsin[[paste0("SUBGRPVAR", s)]] <- dsin[[subgrpvar[s]]]
      # check for numeric codelist variable for the subgroup var, if not create numeric variable
      if (paste0(subgrpvar[s], "N") %in% names(dsin)) {
        dsin[[paste0("SUBGRPVAR", s, "N")]] <- dsin[[paste0(subgrpvar[s], "N")]]
      } else {
        dsin[[paste0("SUBGRPVAR", s, "N")]] <-
          match(dsin[[paste0("SUBGRPVAR", s)]], unique(dsin[[paste0("SUBGRPVAR", s)]]))
      }
      dsin[[paste0("SUBGRPVAR", s)]] <- as.character(dsin[[paste0("SUBGRPVAR", s)]])

      subVa <- paste0("SUBGRPVAR", s)
      subVaN <- paste0("SUBGRPVAR", s, "N")

      # Recode blanks from By and Sub grp vars as Missing
      dsin <- dsin %>%
        mutate(
          across(any_of(c(subVa, subVaN)), ~ if_else(
            . %in% c("", " ", NA, NA_real_),
            if (is.character(.)) "Missing" else 998,
            .
          ))
        )

      # remove missing category value from grouping variable as per user choice
      if (ui_addGrpMiss == "N") {
        dsin <- dsin %>% filter(if_any(
          any_of(c(subVa, subVaN)),
          function(x) !(x %in% c("", NA, "Missing"))
        ))
      }

      # Subgroup total column for the last subgroup variable
      if (ui_sgtotalyn == "Y") {
        dsin <- dsin %>% bind_rows(mutate(dsin, {{ subVa }} := "Total", {{ subVaN }} := 9999))
      }
    }
  }

  # applying population subset selected
  if (!is.na(ui_pop_fil)) {
    if (ui_pop_fil == "Overall Population") {
      dsin <- dsin
    } else {
      dsin <- dsin %>% filter(eval(parse(text = ui_pop_fil)) == "Y")
    }
  }
  # applying denominator/over all subset condition passed by the user
  if (!is.na(ui_dSubset)) {
    dout <- dsin %>% filter(eval(parse(text = ui_dSubset)))
  } else {
    dout <- dsin
  }

  # applying analysis subset condition passed by the user
  if (!is.na(ui_aSubset)) {
    aout <- dout %>% filter(eval(parse(text = ui_aSubset)))
  } else {
    aout <- dout
  }

  ## Calculate big N for mdisplay
  if (ui_bign == "Y") {
    grpVar <- c(var_start(dout, "TRTVAR"), var_start(dout, "SUBGRP"))
    bign <- tibble()
    # big n if treatment exist or subgroup exists
    if (length(grpVar) > 0) {
      for (g in seq_along(grpVar)) {
        bign1 <- dout %>%
          group_by(across(all_of(c(grpVar[1:g])))) %>%
          distinct(USUBJID) %>%
          summarise(BIGN = n())
        bign <- bind_rows(bign, bign1)
      }
    } else {
      # big n if both treatment & Subgroup dont exist
      bign <- length(unique(dout$USUBJID))
    }
  } else {
    # If no big N required
    bign <- NA
  }

  return(list(dsin = aout, dout = dout, bign = bign))
}
