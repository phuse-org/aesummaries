
#' This R Script performs all title & footnotes processing
#'
#' @param summary_by Review the data by count of event or count of absect by event. valid values: "Patients", "Event"
#' @param filters  Adverse Event data filter.
#' @param statistics the inferential statistics to be calculated. valid values: "Risk Ratio", "Risk Difference"
#' @param report Name of the report valid values: Volcano, Forest, event analysis
#'
#' @returna Vector containing 2 values
#'  \itemize{
#'  \item title - Titles to be displayed
#'  \item Footnote - footnotes to be displayed
#'  }
#' @export
#'
#' @examples
#' title_ftnote (summary_by="Patients",
#' filters="",
#' statistics="Risk Ratio",
#' report="Forest")

title_ftnote <- function(summary_by, 
                         filters, 
                         statistics, 
                         report) {

    if (tolower(report) == "volcano") {
    t = paste0("Volcano plot for ", statistics, " of Adverse Events")
    f1 = paste0(
      "* N is the total number of ",
      ifelse(tolower(summary_by)=="patients","participants","events"),
      ". \n",
      "Classifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). \n",
      "Dashed horizontal line represents p-value of 0.05. \n",
      "Dotted horizontal line represents FDR adjusted p-value of approximately 0.05 (when applicable). \n",
      "Dashed Vertical line represents risk value reference line. \n",
      "Totals for the No. of Participants/Events at a higher level are not necessarily the sum of those at the lower levels since a participant may report two or more."
    )
  }
  if (tolower(report) == "forest") {
    t = paste0("Forest plot for ", statistics, " of Adverse Events.")
    f1 = paste0(
      "* N is the total number of ",
      ifelse(tolower(summary_by)=="patients","participants.","events."),
      ". \n",
      "Classifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). \n",
      "Dashed Vertical line represents risk value reference line. \n",
      "Totals for the No. of Participants/Events at a higher level are not necessarily the sum of those at the lower levels since a participant may report two or more."
    )
  }
  if (tolower(report) %in% c("forest","volcano")) {
    if (summary_by == "Events") {
      f2 = "Event counts are the sum of individual occurrences within that category."
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
                  ifelse(filter_name=="","",filter_name),
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
  }
  if(tolower(report)=="event analysis"){
    t=paste0("Event analysis of PT and corresponding FMQ categorization")
    f=paste0("The number of participants reporting at least 1 occurrence of the event specified. \n",
             "* n is the number of participants with adverse events. \n",
             "* N is the total number of participants. \n",
             "Classifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v25.1). \n",
             "FMQ classification is based in FDA FMQ consolidated list. \n",
             "Dashed Horizondal line represents incidence percentage reference line. \n",
             "PT - Preferred Term ; FMQ - FDA MedDRA Queries")
  }
  ft = c(t, f)
  return(ft)
}
