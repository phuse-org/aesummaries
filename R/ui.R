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
ui <- fluidPage(tabsetPanel(
  tabPanel(
    "AE/LB",
    fluid = TRUE,
    column(width = 8, wellPanel(div(
      style = 'height:20px;', fluidRow(div(
        style = 'height:20px;', align = "Left", h4("AE/LB Shiny App")
      ))
    ),)),
    column(width = 2, wellPanel(div(
      style = 'height:20px;', fluidRow(div(style = 'height:20px;',
                                           helpText(
                                             align = "center",
                                             a("Documentation Link", href = "https://github.com/phuse-org/aesummaries/blob/main/README.md")
                                           )))
    ))),
    column(width = 2, wellPanel(div(
      style = 'height:50px;', fluidRow(column(width = 6, div(
        downloadButton('dplot', '', align = 'left')
      )),
      column(width =
               6, div(
                 radioButtons(
                   inputId = "fmt",
                   label = NULL,
                   choices = c("pdf", "html", "pptx")
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
          selectInput("domain", "Domain", choices = c("AE", "LB"))
        ),
        column(
          width = 5,
          style = "font-size: 12px;",
          fluidRow(
            conditionalPanel(condition = 'input.domain=="AE"',
                             selectInput(
                               'report', 'Report', c("Volcano", "Forest")
                             ))
          ),
          conditionalPanel(condition = 'input.domain=="LB"',
                           selectInput('report1', 'Report', c("Edish")))
        ),
        column(
          width = 12,
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
      conditionalPanel(condition = 'input.domain=="LB" && output.fileUploaded',
                       wellPanel(
                         fluidRow(
                           column(
                             width = 12,
                             style = "font-size: 12px;",
                             textInput("subset", "SUBSET", value =
                                         "PARAMCD%in%c('L00028S','L00030S','L00021S')")
                           ),
                           br(),
                           h6('X Axis Options'),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("xbreaks", "Breaks", value =
                                         "0.1,1,2,10")
                           ),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("xlimits", "Limits", value =
                                         "0.1,10")
                           ),
                           br(),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("xticklbl", "Tick Label", value =
                                         "0.1,1,2x ULN,10")
                           ),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("xaxislbl", "Axis Label", value =
                                         "Peak AST or ALT (x ULN)")
                           ),
                           br(),
                           h6('Y Axis Options'),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("ybreaks", "Breaks", value =
                                         "0.1,1,3,10")
                           ),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("ylimits", "Limits", value =
                                         "0.1,10")
                           ),
                           br(),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("yticklbl", "Tick Label", value =
                                         "0.1,1,3x ULN,10")
                           ),
                           column(
                             width = 6,
                             style = "font-size: 12px;",
                             textInput("yaxislbl", "Axis Label", value =
                                         "Peak Bilirubin (x ULN)")
                           ),
                           br(),
                           h6('X Axis Ref Line'),
                           column(
                             width = 4,
                             style = "font-size: 12px;",
                             textInput("rlxintercept", "Intercept", value =
                                         "2")
                           ),
                           column(
                             width = 4,
                             style = "font-size: 12px;",
                             textInput("rlxcolor", "Color", value =
                                         "gray30")
                           ),
                           column(
                             width = 4,
                             style = "font-size: 12px;",
                             textInput("rlxlinetyp", "Line Type", value =
                                         "dashed")
                           ),
                           br(),
                           h6('Y Axis Ref Line'),
                           column(
                             width = 4,
                             style = "font-size: 12px;",
                             textInput("rlyintercept", "Intercept", value =
                                         "3")
                           ),
                           column(
                             width = 4,
                             style = "font-size: 12px;",
                             textInput("rlycolor", "Color", value =
                                         "gray30")
                           ),
                           column(
                             width = 4,
                             style = "font-size: 12px;",
                             textInput("rlylinetyp", "Line Type", value =
                                         "dashed")
                           ),
                           br()
                           
                         )
                       )),
      conditionalPanel(
        condition = 'input.domain=="AE" && output.fileUploaded',
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
                choices = c("Patients", "Events"),
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
          fluidRow(
            style = "font-size: 12px;",
            selectInput(
              "sort_opt",
              "Sorting Option",
              choices = c("Ascending Count", "Descending Count", "Alphabetical"),
              width = "75%"
            ),
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
          br(),
          hr(),
          fluidRow(
            column(
              width = 6,
              style = "font-size: 12px;",
              numericInput(
                "X_ref",
                "X-axis Reference Lines",
                value = 0,
                width = "75%"
              )
            ),
            column(
              width = 6,
              style = "font-size: 12px;",
              selectInput(
                "pvalue_label",
                "P-value Transformation",
                choices = c("None", "-log10"),
                selected = "None",
                width = "75%"
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
        conditionalPanel(condition = 'output.nodata==0',h1('No data to display'))
      )),
      fluidRow(column(
        width = 12,
        conditionalPanel(condition = 'output.nodata>0',
                         div(htmlOutput("footnote_UI"), style = "font-size: 12px; white-space: pre"))
      )),
      fluidRow(column(
        width = 12, DT::dataTableOutput("plot_drill")
      ))
    )
  )
))