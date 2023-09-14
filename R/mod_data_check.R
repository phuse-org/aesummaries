#' data_check UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_check_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          id = ns("box_1"),
          title = tags$strong("Variable Check"),
          maximizable = TRUE,
          width = 12,
          varSelectInput(ns("data_varlist"),
            "Select Variables to be viewed",
            data = NULL,
            selected = NULL,
            multiple = TRUE,
            width = "50vw"
          ),
          div(
            DT::dataTableOutput(ns("sourcedata_display")),
            style = "overflow-x: scroll;"
          )
        )
      ),
      column(
        width = 6,
        box(
          id = ns("box_2"),
          title = tags$strong("Distinct Values Check"),
          maximizable = TRUE,
          width = 12,
          varSelectInput(ns("categorical_variable"),
            "Select Variable to see distinct values",
            data = NULL,
            selected = NULL,
            multiple = TRUE,
            width = "50vw"
          ),
          div(
            DT::dataTableOutput(ns("distinct_value_display")),
            style = "overflow-x: scroll;"
          )
        )
      )
    )
  )
}

#' data_check Server Functions
#'
#' @noRd
mod_data_check_server <- function(id, sourcedata, domain) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(sourcedata())
      req(domain())

      updateVarSelectInput(
        session,
        "data_varlist",
        data = sourcedata()[[domain()]],
        selected = NULL
      )

      updateVarSelectInput(
        session,
        "categorical_variable",
        data = sourcedata()[[domain()]],
        selected = NULL
      )
    })

    output$sourcedata_display <- DT::renderDataTable(
      {
        req(input$data_varlist)

        data_lkup <- sourcedata()[[domain()]] %>%
          select(unlist(strsplit(paste(input$data_varlist, collapse = ","), ",")))

        datatable(
          data_lkup,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = I("colvis"),
            pageLength = 10
          )
        )
      },
      server = FALSE
    )

    output$distinct_value_display <- DT::renderDataTable(
      {
        req(input$categorical_variable)

        if (length(input$categorical_variable) > 0) {
          distinct_value <-
            distinct(sourcedata()[[domain()]], !!!input$categorical_variable)

          datatable(
            distinct_value,
            extensions = "Buttons",
            options = list(
              dom = "Bfrtip",
              buttons = I("colvis"),
              pageLength = 10
            )
          )
        }
      },
      server = FALSE
    )
  })
}
