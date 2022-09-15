################################################################################
# title_ftnote.R
# This R Script performs all title & footnotes processing
################################################################################

#' Title
#'
#' @param summary_by 
#' @param filters 
#' @param statistics 
#' @param report 
#'
#' @return
#' @export
#'
#' @examples

title_ftnote <- function(summary_by, 
                         filters, 
                         statistics, 
                         report) {
  # if(domain=="AE"){
  if (report == "Volcano") {
    t = paste0("Volcano plot for ", statistics, " of Adverse Events")
    f1 = paste0(
      "* N is the total number of ",
      ifelse(tolower(summary_by)=="patients","participants","events"),
      ". \n",
      "Classifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). \n",
      "Dashed horizontal line represents p-value of 0.05 \n",
      "Dotted horizontal line represents FDR adjusted p-value of approximately 0.05 (when applicable) \n",
      "Dashed Vertical line represents risk value reference line \n",
      "Totals for the No. of Participants/Events at a higher level are not necessarily the sum of those at the lower levels since a participant may report two or more"
    )
  }
  if (report == "Forest") {
    t = paste0("Forest plot for ", statistics, " of Adverse Events")
    f1 = paste0(
      "* N is the total number of ",
      ifelse(tolower(summary_by)=="patients","participants","events"),
      ". \n",
      "Classifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). \n",
      "Dashed Vertical line represents risk value reference line \n",
      "Totals for the No. of Participants/Events at a higher level are not necessarily the sum of those at the lower levels since a participant may report two or more"
    )
  }
  if (summary_by == "Events") {
    f2 = "Event counts are the sum of individual occurrences within that category"
    if (is.null(filters)) {
      ftn = "* n is the number of adverse events."
    } else{
      filter_name = tolower(filters)
      if (length(filter_name) <= 1) {
        ftn <-
          paste("* n is the number of ", filter_name, "adverse events.")
      } else if (length(filter_name) > 1) {
        ftn <-
          paste0(
            "* n is the number of ",
            paste0(filter_name[-length(filter_name)], collapse = ", "),
            " and ",
            filter_name[length(filter_name)],
            " adverse events."
          )
      }
    }
  }
  if (summary_by == "Patients") {
    f2 = "The number of participants reporting at least 1 occurrence of the event specified."
    if (is.null(filters)) {
      ftn = "* n is the number of participants with adverse events."
    } else{
      filter_name = tolower(filters)
      if (length(filter_name) <= 1) {
        ftn <-
          paste("* n is the number of participants with ",
                filter_name,
                "adverse events.")
        
      } else if (length(filter_name) > 1) {
        ftn <-
          paste0(
            "* n is the number of participants with ",
            paste0(filter_name[-length(filter_name)], collapse = ", "),
            " and ",
            filter_name[length(filter_name)],
            " adverse events."
          )
      }
    }
  }
  f <- paste0("Note: \n", ftn, " \n", f1, " \n", f2)
  # }
  # if(domain=="LB"){
  #   t=paste0("CaPs TLF Protocol POC","\n","Laboratory Results - eDISH Analysis")
  #   f=paste0("Peak values may not occur at the same timepoint so this graph may differ from Hy's law listing",
  #            "\n","Solid line represent upper limit of normal (ULN)",
  #            "\n","Dashed line represent Hy's law criteria")
  # }
  ft = c(t, f)
  return(ft)
}
