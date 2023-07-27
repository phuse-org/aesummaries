#' Read data from selected path or in-built defaults
#'
#' This function will read data a local path or default; useful for fileInput()
#' object from Shiny
#'
#' @param ui_data_source Source of the input data. Possible Values: "Local", "Default"
#' @param ui_adam_data The data set name; when `ui_data_source` is "Default".
#' When `ui_data_source` = "Local"; List containing 2 elements, `$datapath` (input file path) and
#' `$name` (name of the file/dataset)
#'
#'
#' @return a list containing 2 elements:
#'  \itemize{
#'  \item adam - input data read from the user location as data.frame
#'  \item adam_attrib - data.frame with Variable Names and corresponding labels for `adam`
#'  }
#'
#' @export
#'
#' @examples
#' library(cvars)
#' df <- data_read(ui_data_source = "Default", ui_adam_data = "ADSL")
#' dplyr::slice_head(df$adam$adsl, n = 10)
data_read <- function(
    ui_data_source,
    ui_adam_data) {
  adam <- list()
  adam_attrib <- list()
  ### Reading data from local folders, based on the format of the file loaded in the shiny interface
  if (ui_data_source == "Local") {
    if (is.null(ui_adam_data)) {
      return()
    }
    for (i in seq_along(ui_adam_data$name)) {
      if (file_ext(ui_adam_data$name[i]) == "csv") {
        AnalysisData <- utils::read.csv(ui_adam_data$datapath[i], colClasses = c(
          "SEX" = "character",
          "ASEX" = "character"
        ))
      } else if (file_ext(ui_adam_data$name[i]) == "sas7bdat") {
        AnalysisData <- haven::read_sas(ui_adam_data$datapath[i])
      } else if (file_ext(ui_adam_data$name[i]) == "xls") {
        AnalysisData <- read_excel(ui_adam_data$datapath[i])
      } else if (file_ext(ui_adam_data$name[i]) == "xpt") {
        AnalysisData <- haven::read_xpt(ui_adam_data$datapath[i])
      } else if (tolower(file_ext(ui_adam_data$name[i])) == "rda") {
        AnalysisData <- get(load(ui_adam_data$datapath[i]))
      }
      # converting all the column names to upcase
      names(AnalysisData) <- toupper(names(AnalysisData))
      adam_attrib[[i]] <- data_attrib(datain = AnalysisData)
      adam[[i]] <- AnalysisData
    }

    names(adam) <- file_path_sans_ext(ui_adam_data$name)
    names(adam_attrib) <- file_path_sans_ext(ui_adam_data$name)
  } else if (ui_data_source == "Default") {
    ### Read data from app folders if the data source is default
    ui_adam_data <- unlist(strsplit(ui_adam_data, ","))
    for (i in seq_along(ui_adam_data)) {
      AnalysisData <- haven::read_xpt(paste0(
        app_sys("extdata"), "/",
        tolower(ui_adam_data[i]), ".xpt"
      ))
      # converting all the column names to upcase
      names(AnalysisData) <- toupper(names(AnalysisData))
      adam_attrib[[i]] <- data_attrib(datain = AnalysisData)
      adam[[i]] <- AnalysisData
    }
    names(adam) <- tolower(ui_adam_data)
    names(adam_attrib) <- tolower(ui_adam_data)
  }

  ### returning the analysis dataset and the domain split character
  return(list(adam = adam, adam_attrib = adam_attrib))
}
