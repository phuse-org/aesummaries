#' plot_profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_1"),
      title = tags$strong("Subject Profile"),
      maximizable = TRUE,
      width = 12,
      div(
        uiOutput(ns("profile_plot_title")),
        style = "font-weight: 600;"
      ),
      div(
        plotly::plotlyOutput(ns("profile_plot")),
        style = "height:600px; overflow-y: scroll; display: flex; justify-content: center;"
      )
    )
  )
}

#' plot_profile Server Functions
#'
#' @noRd
mod_plot_profile_server <- function(id, sourcedata, sel_rows, datain, plot_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(sourcedata = NULL, cm_init = TRUE)

    observe({
      if (is.null(sel_rows())) {
        hide("box_1")
      } else {
        show("box_1")
      }
    })

    observe({
      req(sourcedata())
      req(sel_rows())
      req(isTRUE(rv$cm_init))
      if ("cm" %in% names(sourcedata())) {
        rv$sourcedata <- sourcedata()
      }
      src_d <- rv$sourcedata
      if (is.null(rv$sourcedata)) {
        src_d <- sourcedata()
      }
      if (!any(c("cm", "adcm") %in% names(src_d))) {
        showModal(modalDialog(
          title = "Profile: Concomitant Medication not loaded",
          paste0(
            "Upload CM/ADCM data to see overlaid AE-CM profile ",
            "or click 'Close' to proceed with Adverse Events profile"
          ),
          fileInput(
            ns("cm_data"),
            label = "Import CM data (optional)",
            accept = c(".csv", ".sas7bdat", ".xls", ".xpt", ".Rda"),
            multiple = FALSE
          ),
          footer = tagList(actionButton(ns("closeCM"), "Close", class = "sidebar-btn"))
        ))
      }
    }) %>%
      bindEvent(sel_rows())

    observe({
      removeModal()
      if (is.null(input$cm_data)) {
        rv$cm_init <- FALSE
      }
      req(input$cm_data)
      cm_data <- data_read(
        ui_data_source = "Local",
        ui_adam_data = input$cm_data
      )$adam
      rv$sourcedata <- append(sourcedata(), cm_data)
    }) %>%
      bindEvent(input$closeCM)

    observe({
      req(sel_rows())
      req(datain())
      datain <- list(AE = datain())
      dom_names <- names(rv$sourcedata)
      if (any(c("cm", "adcm") %in% dom_names)) {
        datain[["CM"]] <- rv$sourcedata[[dom_names[endsWith(dom_names, "cm")][1]]]
      }
      withProgress(message = "Generating Interval Plot", value = 0, {
        rv$prplot <- multi_interval(
          datain = datain,
          subjectid = plot_data()[sel_rows(), ][["USUBJID"]][1]
        )
      })
    })

    output$profile_plot_title <- renderUI({
      req(rv$prplot)
      HTML(rv$prplot$title)
    })

    output$profile_plot <- plotly::renderPlotly({
      req(rv$prplot)
      rv$prplot$ptly
    })
  })
}
