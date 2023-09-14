#' report_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        box(
          id = ns("box_1"),
          title = tags$strong("Report Selection"),
          maximizable = TRUE,
          width = 12,
          fluidRow(
            column(
              width = 6,
              selectInput(ns("bdomain"),
                "Report base domain",
                choices = NULL,
                width = "25vw"
              ),
              selectInput(ns("tarea"),
                "Therapeutic Area",
                choices = NULL,
                width = "25vw"
              ),
              selectInput(ns("repType"),
                "Report Type",
                choices = NULL,
                width = "25vw"
              )
            ),
            column(
              width = 6,
              selectInput(ns("repName"),
                "Report Name",
                choices = NULL,
                width = "25vw"
              ),
              selectInput(ns("repnum"),
                "Report Number",
                choices = NULL,
                width = "25vw"
              ),
              selectInput(ns("repdesc"),
                "Report Description",
                choices = NULL,
                width = "25vw"
              )
            )
          )
        )
      ),
      column(
        width = 4,
        box(
          id = ns("box_2"),
          title = tags$strong("Treatment & Population Selection"),
          maximizable = TRUE,
          width = 12,
          fluidRow(
            column(
              width = 12,
              selectInput(ns("trt_var"),
                "Treatment Variable",
                choices = NULL,
                selected = NULL
              ),
              selectInput(ns("trt_sort"),
                "Treatment Sort Variable",
                choices = NULL,
                selected = NULL
              ),
              selectInput(ns("popfilter"),
                "Population Filter",
                choices = NULL
              )
            )
          )
        )
      )
    )
  )
}

#' report_selection Server Functions
#'
#' @noRd
mod_report_selection_server <- function(id, sourcedata, domain, data_attr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(sourcedata())
      updateSelectInput(session,
        "bdomain",
        choices = domain(),
        selected = domain()[1]
      )
    })

    report_meta <- reactive({
      req(input$bdomain)

      print("loading report meta begin")
      meta <- get_report_meta()
      print("loading report meta success")
      return(meta)
    })

    observe({
      req(input$bdomain)
      req(report_meta())

      tempRepMeta <- report_meta() %>%
        filter(DOMAIN %in% c(toupper(input$bdomain)))

      updateSelectInput(session,
        "tarea",
        choices = pull(unique(tempRepMeta["TA"]))
      )
    }) %>%
      bindEvent(input$bdomain)

    observe({
      req(input$tarea)

      updateSelectInput(session,
        "repType",
        choices = unique(report_meta()["REPTYPE"][report_meta()["TA"] == input$tarea])
      )
    }) %>%
      bindEvent(input$tarea)

    observe({
      req(input$repType)

      meta <- report_meta()
      choices <-
        unique(meta["REPNAME"][meta["TA"] == input$tarea &
          meta["REPTYPE"] == input$repType])

      updateSelectInput(session,
        "repName",
        choices = choices
      )
    }) %>%
      bindEvent(input$repType)

    observe({
      req(input$repName)

      meta <- report_meta()
      choices_num <- unique(meta["REPNO"][meta["REPNAME"] == input$repName])
      choices_desc <- unique(meta["REPDESC"][meta["REPNAME"] == input$repName])

      updateSelectInput(session,
        "repnum",
        choices = choices_num
      )

      updateSelectInput(session,
        "repdesc",
        choices = choices_desc
      )
    }) %>%
      bindEvent(input$repName)

    observe({
      req(data_attr())
      req(input$repName)

      trtv <-
        c(
          "ARM", "ARMCD", "ACTARM", "ACTARMCD",
          "COHORT", "TRTA", "TRTP", "TRTAN", "TRTPN"
        )
      # Get the treatment variable from the input dataset
      trtvar <- data_attr()[[input$bdomain]] %>%
        filter(VAR_NAMES %in% trtv |
          str_detect(VAR_NAMES, "TRT[:digit:]+[AP]")) %>%
        select(VAR_NAMES) %>%
        pull()

      # Get the Population variable from the input dataset
      popvar <- data_attr()[[input$bdomain]] %>%
        filter(str_detect(tolower(VAR_LABEL), "population")) %>%
        mutate(pop = paste0(VAR_NAMES, " ~ ", VAR_LABEL)) %>%
        select(pop) %>%
        pull()

      updateSelectInput(session,
        "trt_var",
        choices = trtvar,
        selected = trtvar[1]
      )

      updateSelectInput(session,
        "trt_sort",
        choices = trtvar,
        selected = trtvar[1]
      )

      updateSelectInput(session,
        "popfilter",
        choices = c("Overall Population", popvar)
      )
    })

    return(list(
      bdomain = reactive(input$bdomain),
      tarea = reactive(input$tarea),
      repType = reactive(input$repType),
      repName = reactive(input$repName),
      repnum = reactive(input$repnum),
      repdesc = reactive(input$repdesc),
      trt_var = reactive(input$trt_var),
      trt_sort = reactive(input$trt_sort),
      popfilter = reactive(input$popfilter)
    ))
  })
}
