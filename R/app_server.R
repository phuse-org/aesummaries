#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  Sys.sleep(2)
  options(shiny.maxRequestSize = 4096 * 1024^2)

  observe({
    if (input$source == "Default") {
      toggleState("readData", condition = !is.null(input$ADaM_Data_d))
    } else if (input$source == "Local") {
      toggleState("readData", condition = !is.null(input$analysis_data))
    }
  })

  observe({
    if (is.null(input$data_upload)) {
      hide("rep_input")
      hide("gen_filter")
    } else {
      show("rep_input")
      show("gen_filter")
    }
  })

  observe({
    req(rep_inputs$repType())
    if (rep_inputs$repType() == "Figure") {
      choices <- c("html", "pdf", "docx", "pptx", "interactive")
    } else {
      choices <- c("html", "pdf", "docx")
    }

    updateRadioButtons(session,
      "save_fmt",
      choices = choices
    )
  })

  adam_read <-
    mod_data_read_server(
      "data_read_1",
      source = reactive(input$source),
      df_adam = reactive(input$ADaM_Data_d),
      analysis_data = reactive(input$analysis_data),
      read_btn = reactive(input$readData)
    )

  observe({
    if (!is.null(adam_read())) {
      removeUI("#readData")
      runjs("Shiny.setInputValue('data_upload', true);")
    }
  }) %>%
    bindEvent(adam_read())

  rep_inputs <- mod_report_selection_server(
    "report_selection_1",
    sourcedata = reactive(adam_read()$adam),
    domain = reactive(names(adam_read()$adam)),
    data_attr = reactive(adam_read()$adam_attrib)
  )

  mod_data_check_server(
    "data_check_1",
    sourcedata = reactive(adam_read()$adam),
    domain = reactive(rep_inputs$bdomain())
  )

  filters <- mod_generic_filters_server(
    "generic_filters_1",
    sourcedata = reactive(adam_read()$adam),
    domain = reactive(rep_inputs$bdomain()),
    repName = reactive(rep_inputs$repName()),
    repType = reactive(rep_inputs$repType()),
    trt_var = reactive(rep_inputs$trt_var()),
    trt_sort = reactive(rep_inputs$trt_sort()),
    popfilter = reactive(rep_inputs$popfilter())
  )

  tout <-
    mod_toutput_server(
      "toutput_1",
      repName = reactive(rep_inputs$repName()),
      filters = eventReactive(filters(), filters()),
      popfilter = reactive(rep_inputs$popfilter()),
      process_btn = reactive(input$process)
    )

  gout <-
    mod_goutput_server(
      "goutput_1",
      sourcedata = reactive(adam_read()$adam),
      repName = reactive(rep_inputs$repName()),
      filters = eventReactive(filters(), filters()),
      process_btn = reactive(input$process)
    )

  mod_download_report_server(
    "download_report_1",
    repType = reactive(rep_inputs$repType()),
    repName = reactive(rep_inputs$repName()),
    repNum = reactive(rep_inputs$repnum()),
    save_fmt = reactive(input$save_fmt),
    toutput = tout,
    goutput = gout
  )
}
