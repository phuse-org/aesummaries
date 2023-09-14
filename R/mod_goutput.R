#' goutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_goutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("event_box"),
      title = tags$strong("Event Analysis Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 3,
          selectInput(
            ns("hlt_val"),
            "Event Higher Classification",
            choices = NULL
          )
        ),
        column(
          width = 3,
          uiOutput(ns("aeHLT_query_cat_UI"))
        ),
        column(
          width = 3,
          selectInput(
            ns("llt_val"),
            "Event Lower Classification",
            choices = NULL
          )
        ),
        column(
          width = 3,
          uiOutput(ns("aeLLT_query_cat_UI"))
        )
      )
    ),
    box(
      id = ns("box_1"),
      title = tags$strong(htmlOutput(ns("g_title_UI"))),
      maximizable = TRUE,
      width = 12,
      headerBorder = FALSE,
      footer = htmlOutput(ns("g_footnote_UI")),
      div(
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("figure_UI"), width = "80vw"),
          type = 5,
          color = "cadetblue"
        ),
        style = "overflow-x: scroll; height: 650px;"
      )
    ),
    box(
      id = ns("box_2"),
      title = tags$strong("Plot Listing"),
      maximizable = TRUE,
      width = 12,
      DT::dataTableOutput(ns("plot_listing"))
    ),
    mod_plot_profile_ui(ns("plot_profile_1"))
  )
}

#' goutput Server Functions
#'
#' @noRd
mod_goutput_server <- function(id, sourcedata, repName, filters, process_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      statistics = NULL,
      goutput = NULL,
      output_trigger = 0
    )

    observe({
      req(repName())
      req(filters()$ae_pre$dsin)
      req(filters()$ae_pre$dout)
      req(filters()$ae_filter)
      req(filters()$statistics)
      req(filters()$ae_hlt)
      req(filters()$ae_llt)
      req(filters()$summary_by)
      if (tolower(repName()) %in% c("volcano plot")) {
        req(filters()$treatment1)
        req(filters()$treatment1_label)
        req(filters()$treatment2)
        req(filters()$treatment2_label)
      }
      if (tolower(repName()) %in% c("forest plot")) {
        req(filters()$ctrlgrp)
        req(filters()$trtgrp)
        req(filters()$sort_opt)
        req(filters()$sort_by)
      }
      req(filters()$alpha)
      req(filters()$cutoff)
      if (tolower(repName()) %in% c("volcano plot", "forest plot")) {
        print("AE risk_stat process start")
        withProgress(
          rv$statistics <- risk_stat(
            datain = filters()$ae_pre$dsin,
            d_datain = filters()$ae_pre$dout,
            eventVar = ifelse(is.null(filters()$ae_llt), filters()$ae_hlt, filters()$ae_llt),
            summary_by = filters()$summary_by,
            ctrlgrp = ifelse(tolower(repName()) == "volcano plot",
              filters()$treatment1,
              filters()$ctrlgrp
            ),
            trtgrp = ifelse(
              tolower(repName()) == "volcano plot",
              filters()$treatment2,
              paste(filters()$trtgrp, collapse = "~~")
            ),
            statistics = filters()$statistics,
            alpha = filters()$alpha,
            cutoff = filters()$cutoff,
            sort_opt = ifelse(tolower(repName()) == "forest plot", filters()$sort_opt, NA),
            sort_var = ifelse(tolower(repName()) == "forest plot", filters()$sort_by, NA)
          ),
          message = "Executing Get Statistics for EVENTS/ PT...",
          detail = "This step should take a while.",
          min = 0,
          max = 1,
          value = 1
        )
        print("AE risk_stat process end")
        rv$output_trigger <- rv$output_trigger + 1
      }
    }) %>%
      bindEvent(process_btn())

    observe({
      req(tolower(repName()) %in% c("forest plot"))
      req(rv$statistics)
      req(filters()$ae_filter)
      req(filters()$pvalcut)
      req(filters()$X_ref)
      req(filters()$riskScale)
      print("AE Forest Plot process start")
      withProgress(message = "Generating Forest Plot", value = 0, {
        rv$goutput <- try(forest_plot(
          datain = isolate(rv$statistics),
          AE_Filter = filters()$ae_filter,
          review_by = c(filters()$ae_hlt, filters()$ae_llt),
          summary_by = filters()$summary_by,
          statistics = filters()$statistics,
          xref = as.numeric(filters()$X_ref),
          pvalcut = filters()$pvalcut,
          scale_trans = filters()$riskScale
        ))
      })
      print("AE Forest Plot process end")
    }) %>%
      bindEvent(rv$output_trigger)

    observe({
      req(tolower(repName()) %in% c("volcano plot"))
      req(filters()$ae_pre$dsin)
      req(rv$statistics)
      req(filters()$ae_filter)
      req(filters()$statistics)
      req(filters()$treatment1)
      req(filters()$treatment2)
      req(filters()$X_ref)
      req(filters()$summary_by)
      req(filters()$pvalue_label)

      print("AE Volcano Plot process start")
      withProgress(message = "Generating Volcano Plot", value = 0, {
        rv$goutput <- try(volcano_plot(
          datain = isolate(filters()$ae_pre$dsin),
          AE_Filter = filters()$ae_filter,
          statistics_data = isolate(rv$statistics),
          statistics = filters()$statistics,
          treatment1 = filters()$treatment1,
          treatment2 = filters()$treatment2,
          X_ref = as.numeric(filters()$X_ref),
          summary_by = filters()$summary_by,
          pvalue_label = filters()$pvalue_label,
          treatment1_label = filters()$treatment1_label,
          treatment2_label = filters()$treatment2_label,
          pvalcut = filters()$pvalcut
        ))
      })
      print("AE Volcano Plot process end")
    }) %>%
      bindEvent(rv$output_trigger)

    observe({
      req(repName())
      if (tolower(repName()) %in% c("event analysis")) {
        show("event_box")
      } else {
        hide("event_box")
      }
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre$dsin)
      req(filters()$ae_hlt)

      print("AE event analysis hlt list input process start")

      temp1 <- filters()$ae_pre$dsin

      if (filters()$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        if (filters()$ae_hlt %in% names(temp1)) {
          hlt_list <- unlist(strsplit(unique(temp1[[filters()$ae_hlt]]), "~~"))
          hlt_list <-
            sort(unique(gsub("/\\w+", "", hlt_list[!is.na(hlt_list)])))
        } else {
          hlt_list <- NULL
        }
      } else {
        hlt_list <- sort(unique(temp1[[filters()$ae_hlt]]))
      }

      updateSelectInput(
        session,
        "hlt_val",
        "Event Higher Classification",
        choices = hlt_list,
        selected = hlt_list[1]
      )
      print("AE event analysis hlt list input process end")
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre$dsin)
      req(filters()$ae_hlt)
      req(input$hlt_val)
      if (filters()$ae_hlt %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
        req(input$hlt_cat)
      }
      print("AE event analysis llt list input process start")
      temp1 <- filters()$ae_pre$dsin

      if (filters()$ae_hlt %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
        if (toupper(input$hlt_cat) == "NARROW") {
          hl_val1 <- paste0(str_to_upper(input$hlt_val), "/", str_to_upper(input$hlt_cat))
        } else {
          hl_val1 <- input$hlt_val
        }
      } else {
        hl_val1 <- input$hlt_val
      }

      if (filters()$ae_hlt %in% names(temp1) && filters()$ae_llt %in% names(temp1)) {
        llt_list <-
          unique(temp1[[filters()$ae_llt]][str_detect(
            toupper(temp1[[filters()$ae_hlt]]),
            toupper(hl_val1)
          )])
      } else {
        llt_list <- NULL
      }

      if (filters()$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        if (!is.null(llt_list)) {
          llt_list <- unlist(strsplit(unique(llt_list), "~~"))
          llt_list <- sort(unique(gsub("/\\w+", "", llt_list[!is.na(llt_list)])))
        }
      } else {
        llt_list <- sort(llt_list)
      }

      updateSelectInput(
        session,
        "llt_val",
        "Event Term",
        choices = llt_list,
        selected = llt_list[1]
      )
      print("AE event analysis llt list input process end")
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre$dsin)
      print("AE event analysis hlt scope input process start")

      output$aeHLT_query_cat_UI <- renderUI({
        req(filters()$ae_hlt)
        req(filters()$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM"))
        tagList(
          selectInput(
            ns("hlt_cat"),
            "Query Scope",
            choices = c("Narrow", "Broad")
          )
        )
      })
      print("AE event analysis hlt scope input process end")
      print("AE event analysis llt scope input process start")

      output$aeLLT_query_cat_UI <- renderUI({
        req(filters()$ae_llt)
        req(filters()$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM"))
        tagList(
          selectInput(
            ns("llt_cat"),
            "Query Scope",
            choices = c("Narrow", "Broad")
          )
        )
      })
      print("AE event analysis llt scope  input process end")
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre$dsin)
      req(filters()$ae_hlt)
      req(filters()$ae_llt)
      req(input$hlt_val)
      req(input$llt_val)
      if (filters()$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        req(input$hlt_cat)
      }
      if (filters()$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        req(input$llt_cat)
      }
      req(filters()$summary_by)

      print("AE event analysis process start")
      withProgress(message = "Generating AE event analysis", value = 0, {
        rv$goutput <- try(event_analysis(
          datain = filters()$ae_pre$dsin,
          datain_N = filters()$ae_pre$dout,
          hl_var = filters()$ae_hlt,
          hl_val = toupper(input$hlt_val),
          hl_scope = toupper(input$hlt_cat),
          ll_var = filters()$ae_llt,
          ll_val = toupper(input$llt_val),
          ll_scope = toupper(input$llt_cat),
          summary_by = filters()$summary_by,
          ref_line = filters()$ref_line
        ))
      })
      print("AE event analysis process end")
    }) %>%
      bindEvent(process_btn())

    plot_data <- reactive({
      event <- plotly::event_data("plotly_click", source = "plot_output")
      req(length(event) > 0)

      if (tolower(repName()) == "forest plot") {
        test <- rv$goutput$drill_plt$data[as.numeric(event$key), ]
      } else if (tolower(repName()) == "volcano plot") {
        test <- rv$goutput$plot$data[as.numeric(event$key), ]
      } else {
        if (event$curveNumber == 0 && event$x > 0) {
          test <- NULL
        } else {
          test <- rv$goutput$rpt_data
          trt_level_diff <- length(levels(test$TRTVAR)) - length(unique(test$TRTVAR))
          test <- test %>%
            mutate(point_n = as.numeric(as.factor(TRTVAR)) - trt_level_diff)
          order_df <- data.frame(DPTVAL = levels(reorder(test$DPTVAL, -test$DECODh))) %>%
            mutate(curve_n = row_number() + 1)
          test <- full_join(test, order_df, by = "DPTVAL") %>%
            filter(point_n == event$x, curve_n == event$curveNumber)
        }
      }

      req(test)

      display <- filters()$ae_pre$dout %>%
        select(any_of(c(
          "USUBJID", "TRTVAR", "BYVAR1", filters()$ae_llt, "AESER", "AEOUT", "AESEV",
          "AESTDT", "ASTTM", "AEENDT", "TRTSTDT", "TRTEDT", "TRTEMFL"
        )))
      plot_table <- select(test, "BYVAR1", "DPTVAL", "TRTVAR") %>%
        rename(!!filters()$ae_llt := "DPTVAL") %>%
        inner_join(display) %>%
        relocate(c("USUBJID", "TRTVAR"))

      ## displaying the listing table
      plot_table <- plot_table %>%
        rename(
          !!filters()$ae_hlt := "BYVAR1",
          !!filters()$trt_var := "TRTVAR"
        ) %>%
        distinct()
    }) %>%
      bindEvent(plotly::event_data("plotly_click", source = "plot_output"))

    # set selected point to null every time plot updates
    observe({
      req(length(plotly::event_data("plotly_click", source = "plot_output")) > 0)
      runjs("Shiny.setInputValue('plotly_click-plot_output', null);")
    }) %>%
      bindEvent(list(repName(), rv$goutput$drill_plt$data, rv$goutput$plot$data))

    observe({
      if (is.null(plotly::event_data("plotly_click", source = "plot_output"))) {
        hide("box_2")
        runjs("Shiny.setInputValue('goutput_1-plot_listing_rows_selected', null);")
      } else {
        show("box_2")
      }
    })

    output$figure_UI <- plotly::renderPlotly({
      req(rv$goutput)
      req(rv$goutput$ptly)
      rv$goutput$ptly
    })

    output$g_title_UI <- renderText({
      req(rv$goutput$ptly)
      rpt_title <- str_replace_all(rv$goutput$title, "\n", "<br>")
      return(HTML(rpt_title))
    })

    ## footnote for Plot
    output$g_footnote_UI <- renderText({
      req(rv$goutput$ptly)
      rpt_ftnote <- str_replace_all(rv$goutput$footnote, "\n", "<br>")
      return(HTML(rpt_ftnote))
    })

    output$plot_listing <- DT::renderDataTable(
      {
        req(plot_data())

        datatable(
          plot_data(),
          extensions = "Buttons",
          selection = list(mode = "single"),
          options = list(
            dom = "Bfrtip",
            buttons = I("colvis"),
            pageLength = 10,
            scrollX = TRUE
          )
        )
      },
      server = FALSE
    )

    mod_plot_profile_server(
      "plot_profile_1",
      sourcedata = reactive(sourcedata()),
      sel_rows = reactive(input$plot_listing_rows_selected),
      datain = reactive(filters()$ae_pre$dout),
      plot_data = plot_data
    )

    return(reactive(rv$goutput))
  })
}
