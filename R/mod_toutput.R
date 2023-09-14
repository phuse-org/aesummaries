#' toutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_toutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_1"),
      title = tags$strong(htmlOutput(ns("t_title_UI"))),
      maximizable = TRUE,
      width = 12,
      headerBorder = FALSE,
      footer = htmlOutput(ns("t_footnote_UI")),
      div(
        shinycssloaders::withSpinner(uiOutput(ns("table_UI")), type = 5, color = "cadetblue"),
        style = "overflow-x: scroll;"
      )
    )
  )
}

#' toutput Server Functions
#'
#' @noRd
mod_toutput_server <- function(id, repName, filters, popfilter, process_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      toutput = NULL
    )

    observe({
      req(repName())
      req(popfilter())
      req(filters())
      req(tolower(repName()) %in% c("adae_r001"))
      req(filters()$ae_pre)
      req(filters()$ae_filter)
      req(filters()$ae_llt)
      req(filters()$ae_hlt)
      req(filters()$summary_by)
      req(filters()$aeRiskYN)
      if (filters()$aeRiskYN == "Y") {
        req(filters()$treatment1)
        req(filters()$treatment2)
        req(filters()$statistics)
        req(filters()$alpha)
      }
      req(filters()$ui_pctdisp)
      req(filters()$cutoff)
      req(filters()$sort_opt)
      req(filters()$sort_by)
      print("AE Summary table process start")
      withProgress(message = "Generating AE Summary table", value = 0, {
        rv$toutput <- try(
          adae_r001(
            datain = filters()$ae_pre,
            population = str_trim(unlist(strsplit(unique(popfilter()), "~"))[1]),
            AE_Filter = filters()$ae_filter,
            riskyn = filters()$aeRiskYN,
            summary_by = filters()$summary_by,
            ctrlgrp = ifelse(filters()$aeRiskYN == "Y", filters()$treatment1, NA),
            trtgrp = ifelse(filters()$aeRiskYN == "Y", filters()$treatment2, NA),
            ui_lt = filters()$ae_llt,
            ui_ht = filters()$ae_hlt,
            ui_pctdisp = filters()$ui_pctdisp,
            ui_statistics = filters()$statistics,
            ui_alpha = filters()$alpha,
            ui_cutoff = filters()$cutoff,
            ui_sortopt = filters()$sort_opt,
            ui_sortvar = filters()$sort_by
          )
        )
      })
      print("AE Summary table process end")
    }) %>%
      bindEvent(process_btn())

    output$table_UI <- renderUI({
      req(rv$toutput)
      ft <- rv$toutput$tout %>%
        border_inner(fp_border(color = "cadetblue")) %>%
        fontsize(size = 12, part = "header") %>%
        fontsize(size = 9, part = "body")
      htmltools_value(ft)
    })

    output$t_title_UI <- renderText({
      req(rv$toutput)
      rpt_title <- str_replace_all(rv$toutput$title, "\n", "<br>")
      return(HTML(rpt_title))
    })

    output$t_footnote_UI <- renderText({
      req(rv$toutput)
      rpt_ftnote <- str_replace_all(rv$toutput$footnote, "\n", "<br>")
      return(HTML(rpt_ftnote))
    })

    return(reactive(rv$toutput))
  })
}
