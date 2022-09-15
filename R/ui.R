################################################################################
# ui.R
# This R Script only deconflict_preferfines the Web layout for the Shiny App.
################################################################################
# test#
options(shiny.maxRequestSize = 30 * 1024 ^ 4)
options(shiny.reactlog = TRUE)
trtv <- c(
  'TRTA',
  'TRTP',
  'TRTAN',
  'TRTPN',
  'TRT01A',
  'TRT01P',
  'TRT01AN',
  'TRT01PN',
  'ARM',
  'ARMCD',
  'ACTARM',
  'ACTARMCD'
)
ui <- fluidPage(
    column(width = 6, wellPanel(div(
      style = 'height:15px;', fluidRow(div(
        style = 'height:15px;', align = "Left", h4("Advanced R Shiny Interactive AE Plots")
      ))
    ),)),
    column(width = 2, wellPanel(div(
      style = 'height:15px;', fluidRow(div(style = 'height:15px;',style = "font-size: 10px;",
                                           helpText(
                                             align = "center",
                                             a("Documentation Link", href = "https://github.com/phuse-org/aesummaries/blob/main/README.md")
                                           )))
    ))),
    column(width = 4, wellPanel(div(
      style = 'height:15px;', fluidRow(column(width = 2, div(style="display:inline-block;vertical-align:top;height:5px;",
        downloadButton('dplot', '', align = 'left',size = "extra-small")
      )),
      column(width = 10,style = "font-size: 10px;", align='left',div(
                 radioButtons(
                   inputId = "fmt",
                   label = NULL,
                   choices = c("pdf", "html", "pptx","Interactive"),
                   inline=T
                 )
               )))
    ))),
    useShinyjs(),
    column(
      width = 4,
      wellPanel(fluidRow(
        column(
          width = 5,
          style = "font-size: 12px;",
          fluidRow(
                             selectInput(
                               'report', 'Report', c("Forest", "Volcano")
                             )
          ),
        ),
        column(
          width = 5,
          style = "font-size: 12px;",
          selectInput("trt_var", "Treatment Variable", choices = trtv)
        )
      )),
      wellPanel(div(
        style = 'height:120px;',
        fluidRow(
          column(
            width = 5,
            style = "font-size: 12px;",
            selectInput("source", "Data Source", choices = c("Local", "Server"))
          ) ,
          column(
            width = 6,
            style = "font-size: 12px;",
            fluidRow(
              conditionalPanel(
                condition = 'input.source=="Local"',
                fileInput(
                  "analysis_data",
                  label = "Import data",
                  accept = c(".csv", ".sas7bdat",".xpt")
                )
              )
            ),
            conditionalPanel(
              condition = 'input.source=="Server"',
              textInput("server_path", label = "Server Path (only .sas7bdat file)", value = "")
            )
          )
        ),
        fluidRow(column(
          width = 5,
          style = "font-size: 12px;",
          conditionalPanel(condition = "output.fileUploaded", fluidRow(align = "Left", uiOutput("obtain_UI")))
        ))
        
      )),
      conditionalPanel(
        condition = 'output.fileUploaded',
        wellPanel(
          fluidRow(
            column(
              width = 12,
              style = "font-size: 12px;",
              selectInput(
                "period",
                "Period",
                choices = c("Overall Duration", "Other"),
                width = "75%"
              ),
              conditionalPanel(
                condition = 'input.period == "Other"',
                numericInput(
                  "period_please_specify",
                  HTML("Please enter residual period (in days)"),
                  value = 30,
                  min = 0,
                  max = 10 ^ 5
                )
              )
            )
          ),
          fluidRow(column(
            width = 12,
            style = "font-size: 12px;",
            selectInput(
              "ae_filter",
              "Adverse Event filter(s)",
              choices = c(
                "Treatment Emergent",
                "Serious",
                "Drug-related",
                "Mild",
                "Moderate",
                "Severe",
                "Recovered/Resolved",
                "Recovering/Resolving",
                "Not Recovered/Not Resolved",
                "Fatal"
              ),
              selected = NULL,
              multiple = TRUE
            )
          )),
          fluidRow(
            column(
              width = 6,
              style = "font-size: 12px;",
              selectInput(
                "summary_by",
                "Summary By",
                choices = c("Participants"="Patients", "Events"="Events"),
                width = "75%"
              )
            ),
            column(
              width = 6,
              style = "font-size: 12px;",
              selectInput(
                "review_by",
                "Review By",
                choices = c("PT", "SOC"),
                width = "75%"
              )
            ),
          ),
          hr(),
          fluidRow(
            align = "center",
            style = "font-size:12px;",
            sliderInput(
              width = 250,
              "cutoff",
              "Cutoff of Incidence (%)",
              min = 0,
              max = 10,
              value = 5
            )
          ),
          hr(),
          fluidRow(
            style = "font-size: 12px;",
            selectInput(
              "statistics",
              "Measure of Association",
              choices = c("Risk Ratio", "Risk Difference"),
              width = "75%"
            ),
          ),
          hr(),
          conditionalPanel(
            condition='input.report=="Volcano"',
            fluidRow(
              column(
                width = 6,
                style = "font-size: 12px;",
                uiOutput("treatment1_UI"),
                uiOutput("treatment1_label_UI")
              ),
              column(
                width = 6,
                style = "font-size: 12px;",
                uiOutput("treatment2_UI"),
                uiOutput("treatment2_label_UI")
              )
            ),
            hr(),
          ),
          conditionalPanel(
            condition = 'input.report=="Forest"',
            fluidRow(
              style = "font-size: 12px;",
              column(
                width=6,
                selectInput(
                  "sort_opt",
                  "Sorting Option",
                  choices = c("Ascending", "Descending", "Alphabetical")
                )
              ),
              column(
                width=6, 
                selectInput(
                  "sort_by",
                  "Sorting Variable",
                  choices = c("Count", "Percent", "RiskValue")
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 6,
                style = "font-size: 12px;",
                uiOutput("ctrlgrp_UI")
              ),
              column(
                width = 6,
                style = "font-size: 12px;",
                uiOutput("trtgrp_UI")
              )
            ),
            hr()
          ),
          fluidRow(
            column(
              width = 6,
              style = "font-size: 12px;",
              numericInput(
                "X_ref",
                "X-axis Reference Lines",
                value = 0
              )
            ),
            column(
              width = 6,
              style = "font-size: 12px;",
              selectInput(
                "pvalue_label",
                "P-value Transformation",
                choices = c("None", "-log10"),
                selected = "None"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              align = "center",
              width = 6,
              style = "font-size: 12px;",
              numericInput(
                "alpha",
                "Alpha Value(CI)",
                value = 0.05,
                min = 0.01,
                max = 0.1
              )
            ),
            column(
              align = "center",
              width = 6,
              style = "font-size: 12px;",
              numericInput(
                "pvalcut",
                "p Value Cutoff",
                value = 0.05,
                min = 0.01,
                max = 0.1
              )
            )
          )
        )
      )
    )  ,
    column(
      width = 8,
      fluidRow(column(
        width = 12,
        conditionalPanel(condition = 'output.nodata>0',
                         div(htmlOutput("title_UI"), style = "font-size: 12px; white-space: pre"),
        uiOutput("plot_UI")),
        conditionalPanel(condition = 'output.nodata==0',h1('No data to display',style = "text-align: center;padding:200px;"))
      )),
      fluidRow(column(
        width = 12,
        conditionalPanel(condition = 'output.nodata>0',
                         div(htmlOutput("footnote_UI"), style = "font-size: 12px; white-space: pre"))
      )),
      fluidRow(column(
        width = 12, DT::dataTableOutput("plot_drill")
      ))
    ),
    column(width=12,wellPanel(textOutput("version_info_UI"),
                              tableOutput("package_info_UI")))
)
