#' data_read UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_read_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' data_read Server Functions
#'
#' @noRd
mod_data_read_server <-
  function(id, source, df_adam, analysis_data, read_btn) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      df_read <- reactive({
        req(source())
        if (source() == "Default") {
          adam_data <- paste(df_adam(), collapse = ",")
        } else {
          adam_data <- analysis_data()
        }
        adam <- data_read(
          ui_data_source = source(),
          ui_adam_data = adam_data
        )
      }) %>%
        bindEvent(read_btn())
    })
  }
