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
  'ACTARMCD',
  'STUDYID'
)
pop <- c(
  'Overall',
  "Safety",
  "Intent to Treat",
  "Modified Intent to Treat",
  "Per Protocol",
  "Randomised"
)
aeFil<-c(
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
)
ui <- fluidPage(
  # App title Box
  column(
    width = 6, 
    wellPanel(
      div(
        style = 'height:15px;', 
        fluidRow(
          div(
            style = 'height:15px;', 
            align = "Left", 
            h4("Advanced Visual Analytics for Drug Safety Review")
          )
        )
      )
    )
  ), # End of title Box 
  # Begin Download Box
  column(
    width = 6, 
    wellPanel(
      div(
        style = 'height:15px;', 
        fluidRow(
          column(
            width = 2,
            div(
              style="display:inline-block;vertical-align:top;height:5px;",
              downloadButton('dplot', '', align = 'left',size = "extra-small")
            )
          ),
          column(
            width = 10,
            div(
              style = "font-size: 10px;", 
              align='left',
              radioButtons(
                inputId = "fmt",
                label = NULL,
                choices = c("pdf", "html", "pptx","Interactive"),
                inline=T
              )
            )
          )
        )
      )
    )
  ), # End of download box
  # Begin Data loading elements and create report button
  column(
    width=12,
    wellPanel(
      fluidRow(
        # column(
        #   width = 2,
        #   style = "font-size: 12px;",
        #   selectInput("source", "Data Source", choices = c("Local", "Server"))  
        # ) ,
        column(
          width = 3,
          style = "font-size: 12px;",
          fluidRow(
            # conditionalPanel(
            #   condition = 'input.source=="Local"',
              fileInput(
                "analysis_data",
                label = "Import data",
                accept = c(".csv", ".sas7bdat",".xpt",".Rda")
              ),
            # ),
            # conditionalPanel(
            #   condition = 'input.source=="Server"',
            #   textInput("server_path", label = "Server Path (only .sas7bdat file)", value = "")
            # )
          )
        ),
        column(
          width = 2,
          style = "font-size: 12px;",
          selectInput("trt_var", "Treatment Variable", choices = trtv)
        ),
        column(
          width = 3,
          style = "font-size: 12px;",
          selectInput("popfilter", "Population Filter", choices = pop)
        ),
        conditionalPanel(
          condition = "output.fileUploaded", 
          column(
            width=2,
            div(
              style = 'height:6px;font-size: 5px;',
              actionBttn("obtain", "Create", color = "primary", style = 'simple')
            )
          )
        )
      )
    ) # end Data loading elements and create report button
  ),
  column(
    width = 12,
    useShinyjs(),
    # Begin side bar
    tabBox(
      id="tabset1",
      width=NULL,
      tabsetPanel(
        # Begin Overall Tab
        tabPanel(
          "Overall",
          column(
            width = 4,
            # Begin of Volcano and forest plot inputs
            conditionalPanel(
              condition = 'output.fileUploaded',
              wellPanel(
                fluidRow(
                  column(
                    width = 12,
                    style = "font-size: 12px;",
                    selectInput('report', 'Report', c("Forest", "Volcano"))
                  ),
                  column(
                    width = 12,
                    style = "font-size: 12px;",
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
                        max = 10 ^ 5
                      )
                    )
                  ),
                  column(
                    width = 12,
                    style = "font-size: 12px;",
                    selectInput("ae_filter","Adverse Event filter(s)",choices = aeFil,selected = NULL,multiple = TRUE)
                  ),
                  column(
                    width = 6,
                    style = "font-size: 12px;",
                    selectInput(
                      "summary_by",
                      "Summary By",
                      choices = c("Participants"="Patients", "Events"="Events")
                    )
                  )  ,
                  column(
                    width = 6,
                    style = "font-size: 12px;",
                    selectInput("review_by","Review By",choices = c("SOC","PT",'FMQ')) 
                  ),
                  hr(),
                  column(
                    width=8,
                    div(
                      align = "center",
                      style = "font-size:12px;",
                      sliderInput("cutoff","Cutoff of Incidence (%)",min = 0,max = 10,value = 5)
                    )
                  ),
                  hr(),
                  column(
                    width=12,
                    div(
                      style = "font-size: 12px;",
                      selectInput(
                        "statistics",
                        "Measure of Association",
                        choices = c("Risk Ratio", "Risk Difference")
                      )
                    )
                  ),
                  hr(),
                  conditionalPanel(
                    condition='input.report=="Volcano"',
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
                    ),  
                    hr(),
                  ),
                  conditionalPanel(
                    condition = 'input.report != "Volcano"',
                    div(
                      style = "font-
            size: 12px;",
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
            column(
              width = 6,
              style = "font-size: 12px;",
              uiOutput("ctrlgrp_UI")
            ),
            column(
              width = 6,
              style = "font-size: 12px;",
              uiOutput("trtgrp_UI")
            ),
            hr(),
                  ),
            column(
              width = 6,
              style = "font-size: 12px;",
              numericInput("X_ref","X-axis Reference Lines",value = 0)
            ),
            column(
              width = 6,
              style = "font-size: 12px;",
              selectInput("pvalue_label","P-value Transformation",choices = c("None", "-log10"),selected = "None")
            ),
            br(),
            column(
              align = "center",
              width = 6,
              style = "font-size: 12px;",
              numericInput("alpha","Alpha Value(CI)",value = 0.05,min = 0.01,max = 0.1)
            ),
            column(
              width = 6,
              style = "font-size: 12px;",
              numericInput("pvalcut","p Value Cutoff",value = 0.05,min = 0.01,max = 0.1)
            )
                )
              ), # End of Volcano and forest plot inputs
            # Begin of ADAE table input
            wellPanel(
              fluidRow(
                h4("Table Inputs"),
                column(
                  width = 6,
                  style = "font-size: 12px;",
                  textInput("ui_pt","AE Term Variable",value="AEDECOD")
                ),
                column(
                  width = 6,
                  style = "font-size: 12px;",
                  textInput("ui_soc","System Organ Class Variable",value="AEBODSYS")
                ),
                column(
                  width=6,style = "font-size: 12px;",
                  radioButtons(inputId = "ui_trttotyn",label="Total treatment",choices = c("Y","N"),inline = T)
                ),
                column(
                  width=6,style = "font-size: 12px;",
                  radioButtons(inputId = "ui_trtbign",label="Treatment Big N",choices = c("Y","N"),inline = T)
                ),
                column(
                  width=6,style = "font-size: 12px;",
                  selectInput(inputId = "ui_pctdisp",
                              label="Percent Display by",
                              choices=c("Treatment"="TRT","Total"="VAR","SOC"="HT"))
                ),
                column(
                  width=6,uiOutput("trtPairUI")
                )
              )
            )# End of ADAE table input
            )
          ),# End Side Bar
          # Display Panel beginning
          column(
            width = 8,
            tabBox(
              id="tabset2",
              width=NULL,
              tabsetPanel(
                # Begin Graph Tab
                tabPanel("Graph",
                         fluidRow(
                           column(
                             width = 12,
                             conditionalPanel(
                               condition = 'output.nodata>0',
                               div(
                                 htmlOutput("gtitle_UI"), 
                                 style = "font-size: 12px;white-space: pre"),
                               uiOutput("plot_UI")
                             ),
                             conditionalPanel(
                               condition = 'output.nodata==0',
                               h1('No data to display',style = "text-align: center;padding:200px;")
                             )
                           ),
                           column(
                             width = 12,
                             conditionalPanel(
                               condition = 'output.nodata>0',
                               div(
                                 htmlOutput("gfootnote_UI"), style = "font-size: 12px; white-space: pre"
                               )
                             )
                           ),
                           column(
                             width = 12,
                             DT::dataTableOutput("plot_drill")
                           )
                         )
                ),# End graph tab
                # Begin table tab
                tabPanel(
                  "Table",
                  fluidRow(
                    column(
                      width = 12,
                      conditionalPanel(
                        condition = 'output.nodata>0',
                        div(htmlOutput("stitle_UI"), style = "font-size: 12px; white-space: pre"),
                        uiOutput("summary_UI")),
                      column(
                        width = 12,
                        div(htmlOutput("sfootnote_UI"), style = "font-size: 12px; white-space: pre"))
                    )
                  )
                )# End table tab
              )
            )
          )# End of Display Panel
        ),
        # Begin event analysis panel
        tabPanel(
          "Event Analysis",
          conditionalPanel(
            condition = 'output.fileUploaded',
            column(
              width = 12,
              wellPanel(
                fluidRow(
                  column(
                    width = 2,
                    style = "font-size: 12px;",
                    selectInput("query_var","Query Variable",choices = c("FMQ"))
                  ),
                  column(
                    width = 3,
                    style = "font-size: 12px;",
                    uiOutput("query_list_UI")
                  ),
                  column(
                    width = 2,
                    style = "font-size: 12px;",
                    selectInput("query_cat", "Query Scope", choices = c("Narrow","Broad"))
                  ),
                  column(
                    width = 3,
                    style = "font-size: 12px;",
                    uiOutput("PT_list_UI")
                  ),
                  column(
                    width = 2,
                    style = "font-size: 12px;",
                    numericInput("ref_line","Reference Line (%)",min = 0,max = 100,value = 5)
                  ),
                  
                )
              )
            )
          ),
          conditionalPanel(
            condition = 'output.nodata>0',
            fluidRow(
              column( width = 12,
                div(htmlOutput("etitle_UI"), style = "font-size: 12px; white-space: pre")),
              column( width = 12,
                uiOutput("event1_UI")),
              column( width = 12,
                div(htmlOutput("efootnote_UI"), style = "font-size: 12px; white-space: pre"))
              )
            )
        )# End Event analysis panel
      )
    )
  ),
  #Begin Version info display
  column(
    width=12,
    wellPanel(
      textOutput("version_info_UI"),
      tableOutput("package_info_UI")
    )
  )#end version info display
)
