#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      header = bs4Dash::dashboardHeader(
        title = "CVARS",
        status = "white",
        border = TRUE,
        skin = "light",
        bs4Dash::navbarMenu(
          id = "navmenu",
          tooltip(
            navbarTab(
              tabName = "Tab1",
              text = tags$span(icon("gears"), "Report Inputs")
            ),
            title = "Report Inputs"
          ),
          tooltip(
            navbarTab(
              tabName = "Tab2",
              text = tags$span(icon("database"), "Data Check")
            ),
            title = "Data Check"
          ),
          tooltip(
            navbarTab(
              tabName = "Tab3",
              text = tags$span(icon("table-list"), "Table Output")
            ),
            title = "Table Output"
          ),
          tooltip(
            navbarTab(
              tabName = "Tab4",
              text = tags$span(icon("chart-line"), "Graph Output")
            ),
            title = "Graph Output"
          )
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        status = "info",
        skin = "light",
        minified = FALSE,
        width = "300px",
        accordion(
          id = "load_data",
          accordionItem(
            title = tags$strong("Read/Upload Data"),
            collapsed = FALSE,
            selectInput(
              "source",
              "Data Source",
              choices = c(
                "Default (CDISC Pilot Data)" = "Default",
                "Local (Study Data)" = "Local"
              )
            ),
            conditionalPanel(
              condition = 'input.source=="Local"',
              fileInput(
                "analysis_data",
                label = "Import data",
                accept = c(".csv", ".sas7bdat", ".xls", ".xpt", ".Rda"),
                multiple = TRUE
              )
            ),
            conditionalPanel(
              condition = 'input.source=="Default"',
              div(
                HTML(
                  "This option is only available for<br>training and understanding
                  app<br>using CDISC data"
                ),
                style = "text-align:center"
              ),
              selectInput("ADaM_Data_d",
                "CDISC ADaM Data",
                choices = c("ADSL", "ADAE", "CM"),
                multiple = TRUE
              )
            ),
            div(
              tagAppendAttributes(
                actionButton(
                  "readData",
                  "Read Data"
                ),
                class = "sidebar-btn"
              ),
              style = "display: flex; justify-content: center;"
            )
          )
        ),
        conditionalPanel(
          condition = "input.data_upload",
          div(
            tagAppendAttributes(
              actionButton(
                "process",
                "Run Report"
              ),
              class = "sidebar-btn"
            ),
            style = "display: flex; justify-content: center;
            padding-left: 1rem; padding-right: 1rem; padding-bottom: 0.75rem;"
          )
        ),
        conditionalPanel(
          condition = "input.data_upload",
          accordion(
            id = "save_down",
            accordionItem(
              title = tags$strong("Download Report"),
              collapsed = FALSE,
              div(
                radioButtons(
                  "save_fmt",
                  "",
                  choices = "pdf",
                ),
                style = "display:flex; justify-content: center; margin-right: 35px;"
              ),
              div(
                mod_download_report_ui("download_report_1"),
                style = "display:flex; justify-content: center; padding-bottom: 7px;"
              )
            )
          )
        )
      ),
      body = bs4Dash::dashboardBody(
        tabItems(
          tabItem(
            tabName = "Tab1",
            mod_data_read_ui("data_read_1"),
            div(id = "rep_input", mod_report_selection_ui("report_selection_1")),
            div(id = "gen_filter", mod_generic_filters_ui("generic_filters_1"))
          ),
          tabItem(
            tabName = "Tab2",
            mod_data_check_ui("data_check_1")
          ),
          tabItem(
            tabName = "Tab3",
            mod_toutput_ui("toutput_1")
          ),
          tabItem(
            tabName = "Tab4",
            mod_goutput_ui("goutput_1")
          )
        )
      ),
      fullscreen = TRUE,
      dark = NULL,
      help = NULL,
      scrollToTop = TRUE,
      preloader =
        list(
          html = tagList(
            waiter::spin_inner_circles(),
            "Loading CVARS..."
          ),
          color = "#8DCBE6"
        )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cvars"
    ),
    useShinyjs()
  )
}
