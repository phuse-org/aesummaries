#' generic_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_generic_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_1"),
      title = tags$strong("Subsetting Conditions"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          textInput(ns("aeSubset"),
            "Analysis Subset Condition",
            value = "USUBJID!=''"
          )
        ),
        column(
          width = 4,
          textInput(ns("aeDenomSubset"),
            "Denominator Subset Condition",
            value = "STUDYID!=''"
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("ui_pctdisp"),
            label = "Percentage Denominator",
            choices = NULL
          )
        )
      )
    ),
    box(
      id = ns("adae_r001"),
      title = tags$strong("AE Summary Table Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 2,
          radioButtons(
            inputId = ns("aeBigN"),
            label = "Treatment Big N",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 2,
          offset = 1,
          radioButtons(
            inputId = ns("aeTrtTot"),
            label = "Total treatment",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 3,
          radioButtons(
            inputId = ns("aeGrpVarMiss"),
            label = "Add Missing Grouping Variable",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 2,
          offset = 1,
          radioButtons(
            inputId = ns("aeRiskYN"),
            label = "Calculate Risk",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        )
      )
    ),
    box(
      id = ns("box_2"),
      title = tags$strong("Generic Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("ae_filter"),
            "Adverse Event Filter(s)",
            choices = c(
              "Any", "Treatment Emergent", "Serious", "Drug-related", "Mild", "Moderate",
              "Severe", "Recovered/Resolved", "Recovering/Resolving",
              "Not Recovered/Not Resolved", "Fatal"
            ),
            selected = "Any",
            multiple = TRUE
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("period"),
            "Period",
            choices = c("Overall Duration", "Other")
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("period_spec"),
            HTML("Residual period (in days)"),
            value = 30,
            min = 0,
            max = 10^5
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("ae_hlt"),
            "Higher Level Event Term",
            choices = get_ae_term(),
            selected = c("Body System or Organ Class (AEBODSYS)" = "AEBODSYS")
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("ae_llt"),
            "Lower Level Event Term",
            choices = get_ae_term(),
            selected = c("AE Dictionary-Derived Term (AEDECOD)" = "AEDECOD")
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("summary_by"),
            "Summary By",
            choices = c("Participants" = "Patients", "Events" = "Events")
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("statistics"),
            "Measure of Association",
            choices = c("Risk Ratio", "Risk Difference")
          ),
          numericInput(
            ns("X_ref"),
            "Risk Reference Lines",
            value = 0,
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("alpha"),
            "Alpha Value(CI)",
            value = 0.05,
            min = 0.01,
            max = 0.1
          ),
          selectInput(
            ns("riskScale"),
            "Risk Axis Scale",
            choices = c("Log10", "Identity", "Log2")
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("pvalcut"),
            "p Value Cutoff",
            value = 0.05,
            min = 0.01,
            max = 0.1
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          numericInput(
            ns("ref_line"),
            "Reference Line (%)",
            min = 0,
            max = 100,
            value = 5
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          uiOutput(ns("treatment1_UI")),
          uiOutput(ns("treatment1_label_UI")),
          uiOutput(ns("ctrlgrp_UI")),
          selectInput(
            ns("sort_opt"),
            "Sorting Option",
            choices = c("Ascending", "Descending", "Alphabetical")
          )
        ),
        column(
          width = 4,
          uiOutput(ns("treatment2_UI")),
          br(),
          uiOutput(ns("treatment2_label_UI")),
          uiOutput(ns("trtgrp_UI"))
        )
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("sort_by"),
            "Sorting Variable",
            choices = NULL
          )
        ),
        column(
          width = 4,
          sliderInput(
            ns("cutoff"),
            "Cutoff of Incidence (%)",
            min = 0,
            max = 10,
            value = 5
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("pvalue_label"),
            "P-value Transformation",
            choices = c("None", "-log10"),
            selected = "None"
          )
        )
      )
    )
  )
}

#' generic_filters Server Functions
#'
#' @noRd
mod_generic_filters_server <-
  function(id, sourcedata, domain, repName, repType, trt_var, trt_sort, popfilter) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(ae_pre = NULL, ae_pre_comp = 0)

      observe({
        req(domain())
        hide("ui_pctdisp")
        req(repType() == "Table")
        show("ui_pctdisp")

        if (input$aeRiskYN == "N") {
          pct_denom <- c("Treatment" = "TRT", "Total" = "VAR", "High Term" = "HT")
        } else {
          pct_denom <- c("Treatment" = "TRT")
        }

        updateSelectInput(
          session,
          "ui_pctdisp",
          choices = pct_denom
        )
      })

      # observer to control showing/hiding inputs based on report selection
      observe({
        req(domain())
        req(repType())
        req(repName())

        if (input$period == "Other") {
          show("period_spec")
        } else {
          hide("period_spec")
        }

        if (repName() == "adae_r001") show("adae_r001") else hide("adae_r001")

        if (repName() == "Forest Plot" || repName() == "Volcano Plot" ||
          (repName() == "adae_r001") && input$aeRiskYN == "Y") {
          show("statistics")
          show("alpha")
        } else {
          hide("statistics")
          hide("alpha")
        }

        if (repName() == "Forest Plot" || repName() == "Volcano Plot") {
          show("pvalcut")
          show("X_ref")
          show("pvalue_label")
        } else {
          hide("pvalcut")
          hide("X_ref")
          hide("pvalue_label")
        }

        if (repName() == "Forest Plot" || repName() == "adae_r001") {
          show("sort_opt")
          show("sort_by")
        } else {
          hide("sort_opt")
          hide("sort_by")
        }

        if (repName() == "Forest Plot") {
          show("riskScale")
        } else {
          hide("riskScale")
        }

        if (repName() == "Event Analysis") {
          show("ref_line")
          hide("summary_by")
          hide("cutoff")
        } else {
          hide("ref_line")
          show("summary_by")
          show("cutoff")
        }
      })

      observe({
        req(repName())
        req(tolower(repName()) %in% c("forest plot", "adae_r001"))
        req(input$sort_opt != "Alphabetical")

        if (tolower(repName()) == "adae_r001" && input$aeRiskYN == "Y") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(repName()) == "forest plot") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(repName()) == "adae_r001" && input$aeRiskYN == "N") {
          by_var <- c("Count", "Percent")
        }

        updateSelectInput(
          session,
          "sort_by",
          choices = by_var
        )
        print("AE Sort Variable updated")
      })

      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(repType())
        req(trt_var())
        req(trt_sort())
        req(popfilter())
        req(input$ae_filter)
        req(input$ae_hlt)
        if (tolower(domain()) == "adae") {
          print("AE byVar processing start")
          ## evaluating the by variables based on report selection
          if (repType() == "Table") {
            aeByV <- input$ae_hlt
          } else {
            if (tolower(repName()) %in% c("volcano plot", "forest plot", "event analysis")) {
              aeByV <- input$ae_hlt
            } else {
              aeByV <- NA
            }
          }
          print("AE byVar processing end")

          print("AE preprocessing start")
          ### calling Pre Processing AE data
          withProgress(
            rv$ae_pre <- ae_pre_processor(
              datain = sourcedata()[[domain()]],
              ae_filter = input$ae_filter,
              aeSubset = ifelse(repType() == "Table", input$aeSubset, NA),
              aeDenomSubset = ifelse(repType() == "Table", input$aeDenomSubset, NA),
              aeObsPeriod = input$period,
              aeObsResidual = input$period_spec,
              trtvar = toupper(trt_var()),
              trtsort = trt_sort(),
              pop_fil = str_trim(unlist(strsplit(
                unique(popfilter()), "~"
              ))[1]),
              fmq_data = utils::read.csv(paste0(
                app_sys("extdata"), "/FMQ_Consolidated_List.csv"
              )),
              aeEventVar = ifelse(is.null(input$ae_llt), input$ae_hlt, input$ae_llt),
              aeByVar = aeByV,
              aeSubGrpVar = NA,
              aeBigN = ifelse(repType() == "Table", input$aeBigN, "N"),
              aeGrpVarMiss = ifelse(repType() == "Table", input$aeGrpVarMiss, "N"),
              aeTrtTot = ifelse(repType() == "Table", input$aeTrtTot, "N"),
              aeSubGrpTot = "N"
            ),
            message = "Executing pre processing for EVENTS/ PT...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
          print("AE preprocessing end")

          rv$ae_pre_comp <- rv$ae_pre_comp + 1
        }
      }) %>%
        bindEvent(
          list(
            repName(), trt_var(), trt_sort(), popfilter(), input$ae_filter, input$aeSubset,
            input$aeDenomSubset, input$period, input$period_spec, input$ae_hlt, input$ae_llt,
            input$aeBigN, input$aeGrpVarMiss, input$aeTrtTot
          )
        )

      observe({
        req(rv$ae_pre)
        if (tolower(repName()) %in% c("volcano plot", "forest plot") ||
          (tolower(repName()) == "adae_r001" && input$aeRiskYN == "Y")) {
          print("AE treatment pair processing start")

          TRTCD <- unique(rv$ae_pre$dout$TRTVAR[rv$ae_pre$dout$TRTVAR != ""])

          ## Single pair radio button selection for Volcano plot
          output$treatment1_UI <- renderUI({
            req(repName() == "Volcano Plot" || (repName() == "adae_r001") && input$aeRiskYN == "Y")
            radioButtons(
              ns("treatment1"),
              "Control Group",
              choices = TRTCD,
              selected = TRTCD[1]
            )
          })

          output$treatment2_UI <- renderUI({
            req(repName() == "Volcano Plot" || (repName() == "adae_r001") && input$aeRiskYN == "Y")
            radioButtons(
              ns("treatment2"),
              "Treatment Group",
              choices = setdiff(TRTCD, input$treatment1),
              selected = setdiff(TRTCD, input$treatment1)[1]
            )
          })

          output$treatment1_label_UI <- renderUI({
            req(tolower(repName()) == "volcano plot")
            textInput(ns("treatment1_label"),
              "Label for Control Group",
              value = "Control"
            )
          })

          output$treatment2_label_UI <- renderUI({
            req(tolower(repName()) == "volcano plot")
            textInput(ns("treatment2_label"),
              "Label for Treatment Group",
              value = "Treatment"
            )
          })

          ## Multiple pair check box selection for forest plot

          output$ctrlgrp_UI <- renderUI({
            req(tolower(repName()) == "forest plot")
            radioButtons(
              ns("ctrlgrp"),
              "Control Group",
              choices = TRTCD,
              selected = TRTCD[1],
              inline = FALSE
            )
          })

          output$trtgrp_UI <- renderUI({
            req(tolower(repName()) == "forest plot")
            checkboxGroupInput(
              ns("trtgrp"),
              "Treatment Group",
              choices = setdiff(TRTCD, input$ctrlgrp),
              selected = setdiff(TRTCD, input$ctrlgrp)[1]
            )
          })
          print("AE treatment pair processing end")
        }
      }) %>%
        bindEvent(list(rv$ae_pre_comp, input$aeRiskYN))

      filters <- reactive({
        req(rv$ae_pre)
        req(input$ae_filter)
        if ((repName() == "adae_r001") && input$aeRiskYN == "Y") {
          req(input$treatment1)
          req(input$treatment2)
          req(input$statistics)
          req(input$alpha)
        }
        if (repName() == "Volcano Plot") {
          req(input$treatment1)
          req(input$treatment2)
          req(input$treatment1_label)
          req(input$treatment2_label)
        }
        if (tolower(repName()) == "forest plot") {
          req(input$ctrlgrp)
          req(input$trtgrp)
          req(!identical(input$ctrlgrp, input$trtgrp))
        }

        list(
          ae_pre = rv$ae_pre,
          trt_var = trt_var(),
          ae_filter = input$ae_filter,
          ae_hlt = input$ae_hlt,
          ae_llt = input$ae_llt,
          summary_by = input$summary_by,
          aeRiskYN = input$aeRiskYN,
          treatment1 = input$treatment1,
          treatment2 = input$treatment2,
          treatment1_label = input$treatment1_label,
          treatment2_label = input$treatment2_label,
          statistics = input$statistics,
          alpha = input$alpha,
          ui_pctdisp = input$ui_pctdisp,
          cutoff = input$cutoff,
          sort_opt = input$sort_opt,
          sort_by = input$sort_by,
          ctrlgrp = input$ctrlgrp,
          trtgrp = input$trtgrp,
          pvalcut = input$pvalcut,
          riskScale = input$riskScale,
          X_ref = input$X_ref,
          pvalue_label = input$pvalue_label,
          ref_line = input$ref_line
        )
      })

      return(filters)
    })
  }
