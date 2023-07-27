################################################################################
# ui.R
# This R Script only deconflict_preferfines the Web layout for the Shiny App.
################################################################################

###### Shiny data size option and reactlog #####
options(shiny.maxRequestSize = 30 * 1024^4)
options(shiny.reactlog = TRUE)

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#' @noRd

###### Defining Select Choices for Users #########

app_ui <- function(request) {
  data_source <- c(
    "Default (CDISC Pilot Data)" = "Default",
    "Local (Study Data)" = "Local"
  )

  aeTerm <- c(
    "Reported Term for the Adverse Event (AETERM)" = "AETERM",
    "AE Lowest Level Term (AELLT)" = "AELLT",
    "AE Dictionary-Derived Term (AEDECOD)" = "AEDECOD",
    "AE High Level Term (AEHLT)" = "AEHLT",
    "AE High Level Group Term (AEHLGT)" = "AEHLGT",
    "Primary System Organ Class (AESOC)" = "AESOC",
    "Body System or Organ Class (AEBODSYS)" = "AEBODSYS",
    "FMQ Name (FMQ_NAM)" = "FMQ_NAM"
  )

  dashboardPage(
    dashboardHeader(
      title = span("Clinical Visual Analytics for Review and Submission",
        style = "color:#FF9A9E; font-weight:bold;"
      ),
      titleWidth = 600
    ),
    # Begin of Sidebar
    dashboardSidebar(
      fluidRow(
        column( # Begin Data source
          width = 12,
          selectInput("source", "Data Source", choices = data_source),
          conditionalPanel(
            condition = 'input.source=="Local"',
            div("This option is only available", br(),
              "for app developement",
              style = "text-align:center"
            ),
            fileInput(
              "analysis_data",
              label = "Import data",
              accept = c(".csv", ".sas7bdat", ".xls", ".xpt", ".Rda"),
              multiple = TRUE
            )
          ), ### end of local data source
          conditionalPanel(
            condition = 'input.source=="Default"',
            div("This option is only available for", br(),
              "training and understanding app", br(),
              "using CDISC data",
              style = "text-align:center"
            ),
            selectInput("ADaM_Data_d",
              "CDISC ADaM Data",
              choices = c("ADSL", "ADAE"),
              multiple = TRUE
            )
          ), ### end of Default Data source

          uiOutput("readDataBtn_UI"), ### Read data action button placeholder

          conditionalPanel(
            condition = "output.dataUpload",
            ### Buttons to process report inputs , create/refresh graph/table
            actionButton("process", "Process Report Input",
              color = "primary",
              style = "color:#fff;background-color:#337ab7;height:25px;
                         border-color:#2e6da4;font-size:12px;text-align:center;"
            ),
            hr(),
            ### Download button and related file options
            column(
              width = 12,
              downloadButton("save", "SAVE", class = "btn"),
              tags$head(tags$style(".btn{background-color: #337ab7;}"))
            ),
            column(width = 5, background = "black", uiOutput("save_fmt_ui"))
          )
        )
      )
    ), ### End of Sidebar Elements
    ### Begin report save input elements
    ### Begin dashboard body
    dashboardBody(
      ### applying Background color for header, side panel and main panel
      style = ("font-size: 10px; background-color: #212F3D ;"),
      tags$head(tags$style(HTML("
          /* logo */
          .skin-blue .main-header .logo {
                                background-color: #212F3D;
                                }
          /* navbar (rest of the header) */
          .skin-blue .main-header .navbar {
                                background-color: #212F3D; font-size: 10px;
          }
          /* main sidebar */
          .skin-blue .main-sidebar {
                                background-color: #212F3D; font-size: 10px;
                                }
                                "))),
      ### Begin the tab panels
      tabBox(
        id = "tabvals",
        width = NULL,
        tabsetPanel(
          ### Tab to for providing report inputs
          tabPanel(
            "Report Inputs",
            # Report, Treatment & population inputs
            box(
              width = 12,
              style = ("font-size: 10px; background-color: #ECF8F8 ;"),
              collapsible = TRUE,
              title = h5("Report Selection"),
              conditionalPanel(
                condition = "output.dataUpload",
                column(
                  width = 4,
                  uiOutput("report_base_domain_UI"),
                  uiOutput("report_TA_UI"),
                  uiOutput("report_Type_UI")
                ),
                column(
                  width = 4,
                  uiOutput("report_Nam_UI"),
                  uiOutput("report_Num_UI"),
                  uiOutput("report_desc_UI")
                )
              )
            ),
            # Treatment variable and Population selection
            wellPanel(
              style = ("font-size: 10px; background-color: #ECF8F8 ;"),
              fluidRow(
                h5("Treatment & Population Selection"),
                conditionalPanel(
                  condition = "output.dataUpload",
                  uiOutput("treatment_population_UI")
                )
              )
            ),
            # Begin ADAE report Plot Input Elements
            conditionalPanel(
              condition = 'output.dataUpload && input.bdomain=="adae"',
              wellPanel(
                style = ("font-size: 10px; background-color: #ECF8F8 ;"),
                fluidRow(
                  column(
                    width = 3,
                    textInput("aeSubset", "Analysis Subset Condition", value = "USUBJID!=''")
                  ),
                  column(
                    width = 3,
                    textInput("aeDenomSubset",
                      "Denominator Subset Condition",
                      value = "STUDYID!=''"
                    )
                  ),
                  conditionalPanel(
                    condition = 'output.repName=="adae_r001"',
                    column(
                      width = 3,
                      uiOutput("pct_disp_UI")
                    )
                  )
                ),
                conditionalPanel(
                  condition = 'output.repName=="adae_r001"',
                  fluidRow(
                    column(
                      width = 3,
                      radioButtons(
                        inputId = "aeBigN",
                        label = "Treatment Big N",
                        choices = c("Y", "N"),
                        selected = "N",
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 3,
                      radioButtons(
                        inputId = "aeTrtTot",
                        label = "Total treatment",
                        choices = c("Y", "N"),
                        selected = "N",
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 3,
                      radioButtons(
                        inputId = "aeGrpVarMiss",
                        label = "Add Missing Grouping Variable",
                        choices = c("Y", "N"),
                        selected = "N",
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 3,
                      radioButtons(
                        inputId = "aeRiskYN",
                        label = "Calculate Risk",
                        choices = c("Y", "N"),
                        selected = "N",
                        inline = TRUE
                      )
                    )
                  )
                ),
                fluidRow(
                  column(width = 12, hr(style = "border-top: 1px solid #000000;")),
                  column(
                    width = 4,
                    uiOutput("AE_Event_Filter_UI"),
                    selectInput(
                      "period",
                      "Period",
                      choices = c("Overall Duration", "Other")
                    ),
                    conditionalPanel(
                      condition = 'input.period == "Other"',
                      numericInput(
                        "period_please_specify",
                        HTML("Please enter residual period (in days)"),
                        value = 30,
                        min = 0,
                        max = 10^5
                      )
                    )
                  ),
                  column(
                    width = 4,
                    conditionalPanel(
                      condition = 'output.repName=="forest plot" || output.repName=="volcano plot"
                      || output.repName=="tornado plot"||output.repName=="adae_r001"',
                      selectInput(
                        "summary_by",
                        "Summary By",
                        choices = c("Participants" = "Patients", "Events" = "Events")
                      ),
                    ),
                    selectInput("ae_hlt", "Higher Level Event Term",
                      choices = aeTerm,
                      selected = c("Body System or Organ Class (AEBODSYS)" = "AEBODSYS")
                    ),
                    conditionalPanel(
                      condition = 'output.repName!=="tornado plot"',
                      selectInput("ae_llt", "Lower Level Event Term",
                        choices = aeTerm,
                        selected = c("AE Dictionary-Derived Term (AEDECOD)" = "AEDECOD")
                      )
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot" || output.repName=="volcano plot"
                      || (output.repName=="adae_r001" && input.aeRiskYN=="Y")',
                      selectInput(
                        "statistics",
                        "Measure of Association",
                        choices = c("Risk Ratio", "Risk Difference")
                      )
                    )
                  ),
                  column(
                    width = 4,
                    style = "border-left: 1px solid",
                    conditionalPanel(
                      condition =
                        'output.repName=="forest plot" || output.repName=="volcano plot" ||
                      (output.repName=="adae_r001" && input.aeRiskYN=="Y")',
                      numericInput(
                        "alpha",
                        "Alpha Value(CI)",
                        value = 0.05,
                        min = 0.01,
                        max = 0.1
                      )
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot" || output.repName=="volcano plot"',
                      numericInput(
                        "pvalcut",
                        "p Value Cutoff",
                        value = 0.05,
                        min = 0.01,
                        max = 0.1
                      ),
                      numericInput(
                        "X_ref",
                        "Risk Reference Lines",
                        value = 0,
                      )
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot"',
                      selectInput(
                        "riskScale",
                        "Risk Axis Scale",
                        choices = c("Log10", "Identity", "Log2")
                      )
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="event analysis"',
                      numericInput("ref_line", "Reference Line (%)", min = 0, max = 100, value = 5)
                    )
                  ),
                  column(width = 12, hr())
                ),
                fluidRow(
                  column(
                    width = 4,
                    conditionalPanel(
                      condition = 'output.repName=="volcano plot"||
                      (output.repName=="adae_r001" && input.aeRiskYN=="Y")',
                      uiOutput("treatment1_UI"),
                      br()
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot"',
                      uiOutput("ctrlgrp_UI"),
                      br(),
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot"||output.repName=="adae_r001"',
                      selectInput(
                        "sort_opt",
                        "Sorting Option",
                        choices = c("Ascending", "Descending", "Alphabetical")
                      )
                    )
                  ),
                  column(
                    width = 4,
                    conditionalPanel(
                      condition = 'output.repName=="volcano plot"||
                      (output.repName=="adae_r001" && input.aeRiskYN=="Y")',
                      uiOutput("treatment2_UI"),
                      br()
                    ),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot"',
                      uiOutput("trtgrp_UI"),
                      br(),
                      br(),
                    ),
                    conditionalPanel(
                      condition = 'output.repName!=="volcano plot"',
                      uiOutput("aeSortBy_UI")
                    )
                  ),
                  column(
                    width = 4,
                    style = "border-left: 1px solid",
                    conditionalPanel(
                      condition = 'output.repName=="forest plot" ||
                      output.repName=="volcano plot" || output.repName=="adae_r001" ',
                      sliderInput(
                        "cutoff",
                        "Cutoff of Incidence (%)",
                        min = 0,
                        max = 10,
                        value = 5
                      )
                    ),
                    br(),
                    conditionalPanel(
                      condition = 'output.repName=="forest plot" ||
                      output.repName=="volcano plot"',
                      selectInput(
                        "pvalue_label",
                        "P-value Transformation",
                        choices = c("None", "-log10"),
                        selected = "None"
                      )
                    )
                  )
                )
              )
            )
          ), # End of Report input tab
          ### Tab to view analysis data loaded to shiny app
          tabPanel(
            "Data Check",
            fluidRow(
              conditionalPanel(
                condition = "output.dataUpload",
                box(
                  width = 12,
                  style = ("font-size: 10px; background-color: #ECF8F8 ;"),
                  uiOutput("source_data_varlist_UI"),
                  column(
                    width = 12,
                    style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                    DT::dataTableOutput("sourcedata_display")
                  ),
                  uiOutput("categorical_variable_list_UI"),
                  DT::dataTableOutput("distinct_value_display")
                )
              )
            )
          ),
          # Begin  graph display and input element
          tabPanel(
            "Graph Output",
            conditionalPanel(
              condition = "output.g_output",
              column(
                style = ("font-size: 10px; background-color: #ECF8F8 ;"),
                width = 12,
                div(htmlOutput("g_title_UI"), style = "white-space: pre"),
                div(uiOutput("figure_UI")),
                div(htmlOutput("g_footnote_UI"), style = "white-space: pre")
              )
            ),
            wellPanel(
              style = ("font-size: 10px; background-color: #ECF8F8 ;"),
              fluidRow(
                column(
                  width = 12,
                  conditionalPanel(
                    condition = 'output.repName=="event analysis"',
                    column(width = 3, uiOutput("aeHLT_list_UI")),
                    column(width = 3, uiOutput("aeHLT_query_cat_UI")),
                    column(width = 3, uiOutput("aeLLT_list_UI")),
                    column(width = 3, uiOutput("aeLLT_query_cat_UI"))
                  ),
                  conditionalPanel(
                    condition = 'output.repName=="forest plot"||output.repName=="volcano plot"',
                    DT::dataTableOutput("plot_listing")
                  )
                )
              )
            )
          ), # End  graph display and input element
          # Begin Table display Tab and Input element
          tabPanel(
            "Table Output",
            conditionalPanel(
              condition = "output.t_output",
              wellPanel(
                style = ("font-size: 10px; background-color: #ECF8F8 ;"),
                fluidRow(
                  column(
                    width = 12,
                    div(htmlOutput("t_title_UI"), style = "white-space: pre"),
                    div(uiOutput("table_UI")),
                    div(htmlOutput("t_footnote_UI"), style = "white-space: pre")
                  )
                )
              )
            )
          ) # End Table display Tab and Input element
        ) # End of tabset panel
      ) # End of tab box
    ) # end of dashboard body
  ) # end of dashboard page
}
