#' Display standard tables
#'
#'' mdisplay' is used to generate flextable output of standardised tables
#'
#' @details
#' This generic function takes data input from standard Mcatstat/Msumstat utilities.
#' The resulting flextable is sorted and displayed according to standard variable names,
#' including variable DPTVARN for sorting blocks and DPTVALN for sorting inside blocks.
#'
#' @param datain Input data
#' @param ui_bylabel Change BYVAR names to label
#' @param ui_catlabel Change cat/dptvar names to label
#' @param ui_statlabel Change statistics names to label
#' @param trtbign Display big N or not
#' @param extra_df Additional dataframe merge by variables given in extra_mergeby
#' @param extra_mergeby Variables to merge extra_df by, if present
#' @param extra_vars Variables additional to standard present in input data to display at last
#' @param colformat String (usually indicating format) to add to column (treatment/total) names
#' @param indent List of conditions to indent rows, each element increases padding.
#' eg list("DPTVALN==3","DPTVARN==1")
#' @param dptlabel String to become name of the column containing categories (DPTVAL) in output
#'
#' @return a list containing following objects
#' \itemize{
#' \item tout - Flextable output to be displayed or downloaded
#' \item dataf - Final dataframe passed to flextable
#' }
#' @export
#'
#' @examples
#' library(cvars)
#' data("ae_pre")
#' # mcatstat output will the input for mdisplay
#' mcat_out <- mcatstat(
#'   datain = ae_pre$dsin,
#'   d_datain = ae_pre$dout,
#'   ui_uniqid = "USUBJID",
#'   ui_dptvar = "AEDECOD",
#'   ui_pctdisp = "TRT",
#'   miss_catyn = "N",
#'   cum_ctyn = "N",
#'   total_catyn = "N",
#'   dptvarn = 1
#' )
#'
#' out <- mdisplay(
#'   datain = mcat_out,
#'   ui_bylabel = NA,
#'   ui_catlabel = NA,
#'   ui_statlabel = NA,
#'   trtbign = NA,
#'   extra_df = NULL,
#'   extra_mergeby = "DPTVAL",
#'   extra_vars = "",
#'   colformat = "",
#'   indent = NULL,
#'   dptlabel = "  "
#' )
#'
#' out$tout
#'
mdisplay <- function(datain,
                     ui_bylabel = NA,
                     ui_catlabel = NA,
                     ui_statlabel = NA,
                     trtbign = NA,
                     extra_df = NULL,
                     extra_mergeby = "DPTVAL",
                     extra_vars = "",
                     colformat = "",
                     indent = NULL,
                     dptlabel = "  ") {
  # Identify if by groups exist
  BYVAR <- var_start(datain, "BYVAR")
  # Identify subgroups if exists
  SUBGRP <- var_start(datain, "SUBGRP")
  # Byvar N variable if exists
  BYVARN <- var_start(datain, "BYVARN")
  SUBGRPN <- var_start(datain, "SUBGRPVARN")
  # Standardise variable names and select
  report1 <- datain %>%
    rename(any_of(c("DPTVAL" = "STAT", "DPTVALN" = "STATN"))) %>%
    select(
      DPTVAR, any_of(c("TRTVAR", SUBGRP, BYVAR, BYVARN, SUBGRPN, "DPTVARN", "DPTVALN", extra_vars)),
      DPTVAL, CVALUE, CN
    ) %>%
    mutate(CVALUE = as.character(CVALUE))


  ## If treat or subgrp are present
  if ("TRTVAR" %in% names(report1) ||
    "SUBGRPVAR1" %in% names(report1)) {
    # If bign is required (i.e, a non NA output from mentry)
    if (!all(is.na(trtbign))) {
      # bign for both grouping variables, treatment and subgroup
      Grp <- intersect(c("TRTVAR", SUBGRP), names(trtbign))
      for (g in seq_along(Grp)) {
        d <- trtbign %>% filter(if_any(any_of(Grp[g]), function(x) !is.na(x)))
        if (g != length(Grp)) {
          d <- d %>%
            filter(if_any(any_of(Grp[g + 1]), function(x) is.na(x))) %>%
            select(BIGN, all_of(Grp[1:g]))
        }
        report1 <- report1 %>% left_join(d, by = Grp[1:g])
        report1[[paste0(Grp[g], "_BIGN")]] <- paste0(
          report1[[Grp[g]]],
          " (N=", report1[["BIGN"]], ")", colformat
        )
        if (Grp[g] == "TRTVAR") {
          report1 <- report1 %>% arrange(TRTVAR)
          report1[["TRTVAR_BIGN"]] <- fct_inorder(report1[["TRTVAR_BIGN"]])
        }
        report1 <- report1 %>% select(-BIGN)
      }

      report1 <- report1 %>%
        ungroup() %>%
        select(-any_of(Grp))
      colnames(report1) <- sub("\\_BIGN", "", colnames(report1))
    }

    # Insert workaround to bypass flextable bug for non unique column names in span_header
    if (length(SUBGRP) > 0) {
      for (i in seq_along(SUBGRP)) {
        report1[[SUBGRP[i]]] <- paste0(report1[[SUBGRP[i]]], paste(rep(" ", i), collapse = ""))
      }
    }
    # transponse wider to get treatment_subgrp variables ordered by subgrpn
    rep <- report1 %>%
      arrange(across(any_of(c("TRTVAR", SUBGRPN)))) %>%
      select(-any_of(SUBGRPN)) %>%
      tidyr::pivot_wider(
        names_from = c(any_of("TRTVAR"), all_of(SUBGRP)),
        values_from = CVALUE,
        values_fill = "-",
        names_sort = FALSE
      ) %>%
      ungroup() %>%
      mutate(across(!BYVAR & !DPTVAR & !DPTVAL & where(is.character), function(x) {
        ifelse(CN == "C", gsub("-", "0", x), x)
      }))
  } else {
    # If Treatment is selected as None: Display a total column
    rep <- report1 %>%
      ungroup() %>%
      rename("Total" = "CVALUE")
    if (!all(is.na(trtbign))) {
      rep <- rep %>% rename(!!paste0("Total (N=", trtbign, ")", colformat) := "Total")
    }
  }

  rep <- rep %>% select(BYVAR, DPTVAR, DPTVAL, everything())

  # If additional dataset is given:
  if (!is.null(extra_df)) {
    rep <- rep %>% inner_join(extra_df, by = extra_mergeby)
  }


  # Labels for Stats
  if (!is.na(ui_statlabel)) {
    statn <- setNames(
      unlist(str_split(ui_statlabel, ",")),
      unique(rep$DPTVAL[rep$CN == "N"])
    )
    rep <- rep %>%
      mutate(
        Newstat = recode(DPTVAL, !!!statn),
        DPTVAL = ifelse(CN == "N", Newstat, DPTVAL)
      ) %>%
      select(-Newstat)
  }

  # order of byvar and by labels
  if (length(BYVAR) > 0) {
    # Arrange rows per byvarn and dptvarn, dptvaln as given by user
    repdat <- rep %>%
      arrange(across(any_of(c(BYVARN, "DPTVARN", "DPTVALN")))) %>%
      select(-any_of(BYVARN))

    if (length(unique(repdat$DPTVARN)) != 1) {
      lenmerge <- length(BYVAR) + 1
    } else {
      lenmerge <- length(BYVAR)
    }
    VMERGE <- 1
    for (iby in 1:lenmerge) {
      if (iby != 1) {
        block <- repdat %>%
          select(all_of(1:iby)) %>%
          tidyr::unite(BLOCK, sep = "-") %>%
          rename(!!paste0("BLOCK", iby) := "BLOCK")

        repdat <- cbind(repdat, block)
        VMERGE <- c(VMERGE, which(colnames(repdat) == paste0("BLOCK", iby)))
      }
    }
    # Combine rows in by columns:
    VTARG <- c(1:seq_along(BYVAR), length(BYVAR) + 1)

    # Apply label to BYVAR columns, if given
    if (all(!is.na(ui_bylabel))) {
      bylabel <- unlist(str_split(ui_bylabel, ","))
      repdat <- repdat %>% rename(any_of(setNames(BYVAR, bylabel)))
    }
  } else {
    repdat <- rep %>% arrange(DPTVARN, DPTVALN)
    VMERGE <- which(colnames(repdat) == "DPTVARN") # Changed
    VTARG <- 1
  }

  # If any rows are made as DPTVALN=0, will become bold
  vals <- which(colnames(repdat) == "DPTVAL")
  boldrows <- which(repdat$DPTVALN == 0)

  # Labels for categories
  if (!is.na(ui_catlabel)) {
    catlabel <- setNames(unlist(str_split(ui_catlabel, ",")), unique(repdat$DPTVAR))
    repdat <- repdat %>%
      mutate(DPTVAR = recode(DPTVAR, !!!catlabel))
  }


  ## placeholder: convert PAGE_BYs to factor and then
  ## group_split into list
  ## for label concatenate page by vars and save as name of list

  # If Additional Variables are present (extra_Vars), need to be at the end of dataset:
  if (extra_vars %in% colnames(repdat)) {
    repdat <- repdat %>% relocate(extra_vars, .after = last_col())
  }

  # final output
  chktarg <- 1
  if (dptlabel == "") dptlabel <- "  "
  if (all(repdat$DPTVAR == "" | repdat$DPTVAR == " ")) {
    repdat1 <- repdat %>% rename(!!dptlabel := DPTVAL)
    reqcols <- names(repdat1)[!(names(repdat1) %in%
      c("DPTVAR", "DPTVARN", "CN", "DPTVALN") | startsWith(names(repdat1), "BLOCK"))]

    if (length(VTARG) > 1) VTARG <- VTARG[-length(VTARG)] else chktarg <- 0
    vals <- vals - 1
  } else {
    repdat1 <- repdat %>% rename(!!dptlabel := DPTVAL, " " = DPTVAR)
    reqcols <- names(repdat1)[!(names(repdat1) %in%
      c("DPTVARN", "CN", "DPTVALN") | startsWith(names(repdat1), "BLOCK"))]
  }


  repout <- repdat1 %>%
    flextable(col_keys = reqcols) %>%
    ftExtra::span_header(sep = "_") %>%
    fontsize(size = 7, part = "all")

  if (chktarg == 1) {
    repout <- repout %>%
      merge_v(j = VMERGE, target = VTARG) %>%
      valign(j = VTARG, valign = "top", part = "body") %>%
      bold(j = VTARG)
  }
  if (length(boldrows) > 0) {
    repout <- repout %>% bold(i = boldrows, j = vals)
  }

  # Indenting rows if needed:
  if (!is.null(indent)) {
    pad <- 0
    for (x in indent) {
      pad <- pad + 20
      x_id <- with(repdat, which(eval(parse(text = x))))
      repout <- repout %>% padding(i = x_id, j = vals, padding.left = pad)
    }
  }

  repout <- repout %>%
    theme_box() %>%
    align(align = "center", part = "header") %>%
    autofit()

  return(list(tout = repout, dataf = repdat))
}
