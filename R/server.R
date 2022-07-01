################################################################################
# server.R
# This R Script defines the server logics for the Shiny App.
################################################################################

server <- function(input, output, session) {
  data <- reactiveValues(
    domain = NULL,
    data_in = NULL,
    statistics = NULL,
    statistics_soc = NULL
  )
  
  plots <- reactiveValues(plot_data = NULL,
                          plot_output = NULL,
                          plot_click = NULL)
  
  ### Data uploading -----------------------------------------------------------
  
  analysis_data <- reactive({
    # SDTM
    withProgress(
      df <- data_processing(
        datain = input$analysis_data,
        domain = input$domain,
        data_source = input$source,
        server_path = input$server_path,
        data_filter = input$ae_filter,
        trtvar = input$trt_var,
        obs_period = input$period,
        obs_residual = input$period_please_specify
      ),
      message = "If reading data from server...",
      detail = "This step should take a while.",
      min = 0,
      max = 1,
      value = 1
    )
  })
  
  # once data is loaded, all other inputs are loaded
  output$fileUploaded <-
    reactive({
      return(!is.null(analysis_data()))
    })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  domain <- reactive(input$domain)
  observe({
    #only runs for SDTM
    if (!is.null(analysis_data())) {
      data$data_in <- analysis_data()
    }
  })
  
  ### Extract all arms ---------------------------------------------------------
  ARMCD <- reactive({
    req(data$data_in)
    print('code check')
    return(unique(data$data_in$TRTVAR[data$data_in$TRTVAR != ""]))
  })
  
  ### Definitions of UI outputs in the control panel and related logics --------
  output$review_by_please_specify_UI <- renderUI({
    req(data$data_in)
    req(input$review_by)
  })
  
  
  output$treatment1_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    div(
      style = 'height:100px; width:91%; overflow: scroll',
      checkboxGroupInput(
        "treatment1",
        "Group A",
        choices = ARMCD(),
        selected = ARMCD()[1]
      )
    )
  })
  
  output$treatment2_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    div(
      style = 'height:100px; width:91%; overflow: scroll',
      checkboxGroupInput(
        "treatment2",
        "Group B",
        choices = setdiff(ARMCD(), input$treatment1),
        selected = setdiff(ARMCD(), input$treatment1)[1]
      )
    )
  })
  
  output$treatment1_label_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    textInput("treatment1_label",
              "Label for Group A",
              value = "Unexposed",
              width = "75%")
  })
  
  output$treatment2_label_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    textInput("treatment2_label",
              "Label for Group B",
              value = "Exposed",
              width = "75%")
  })
  
  ### An essential step for obtaining statistics right after data uploading and before the generation of plot ------------------------------------
  observeEvent(input$obtain, {
    if (input$domain == "AE") {
      if (input$period == "Other") {
        req(input$period_please_specify)
      }
      if (input$review_by != "SOC") {
        if (!is.null(analysis_data())) {
          data$statistics <- analysis_data()
        } 
      }
      if (input$summary_by == "Events") {
        if (input$review_by != "SOC") {
          withProgress(
            data$statistics <- GetStatistics_all(
              data = analysis_data(),
              review_by = input$review_by,
              summary_by = input$summary_by,
              treatment1 = input$treatment1,
              treatment2 = input$treatment2,
              statistics = input$statistics,
              alpha = input$alpha,
              cutoff = input$cutoff,
              sort_opt = input$sort_opt
            ),
            message = "Executing Get Statistics for EVENTS/ PT...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
        } else if (input$review_by == "SOC") {
          withProgress(
            data$statistics <- GetStatistics_all(
              data = analysis_data(),
              review_by = input$review_by,
              summary_by = input$summary_by,
              treatment1 = input$treatment1,
              treatment2 =
                input$treatment2,
              statistics = input$statistics,
              alpha = input$alpha,
              cutoff = input$cutoff,
              sort_opt = input$sort_opt
            ),
            message = "Executing Get Statistics for EVENTS/ SOC...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
        }
      } else if (input$summary_by == "Patients") {
        if (input$review_by != "SOC") {
          withProgress(
            data$statistics <- GetStatistics_all(
              data = analysis_data(),
              review_by = input$review_by,
              summary_by = input$summary_by,
              treatment1 = input$treatment1,
              treatment2 =
                input$treatment2,
              statistics = input$statistics,
              alpha = input$alpha,
              cutoff = input$cutoff,
              sort_opt = input$sort_opt
            ),
            message = "Executing Get Statistics for PATIENTS/ PT...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
        } else if (input$review_by == "SOC") {
          withProgress(
            data$statistics <- GetStatistics_all(
              data = analysis_data(),
              review_by = input$review_by,
              summary_by = input$summary_by,
              treatment1 = input$treatment1,
              treatment2 =
                input$treatment2,
              statistics = input$statistics,
              alpha = input$alpha,
              cutoff = input$cutoff,
              sort_opt = input$sort_opt
            ),
            message = "Executing Get Statistics for EVENTS/SOC...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
        }
      }
      if (input$report == "Volcano") {
        withProgress(message = 'Generating Volcano Plot', value = 0, {
          out <- try(volcano_plot(
            data = isolate(data$data_in),
            statistics_data = isolate(data$statistics),
            statistics = input$statistics,
            treatment1 = input$treatment1,
            treatment2 = input$treatment2,
            X_ref = as.numeric(input$X_ref),
            review_by = input$review_by,
            summary_by = input$summary_by,
            pvalue_label = input$pvalue_label,
            treatment1_label = input$treatment1_label,
            treatment2_label = input$treatment2_label
          ))
        })
      } else if (input$report == "Forest") {
        withProgress(message = 'Generating Forest Plot', value = 0, {
          out <- try(Forest_Plot(
            dat = isolate(data$statistics),
            groupvar = TRTVAR,
            review_by = input$review_by,
            summary_by = input$summary_by,
            statistics = input$statistics,
            xlims = c(0, 3),
            xref = as.numeric(input$X_ref)
          ))
        })
      }
    }
    if (input$domain == "LB") {
      withProgress(message = 'Generating Edish Plot', value = 0, {
        out <- try(edish(
          datain = analysis_data(),
          subset = input$subset,
          xaxisopt = c(
            input$xbreaks,
            input$xlimits,
            input$xticklbl,
            input$xaxislbl
          ),
          yaxisopt = c(
            input$ybreaks,
            input$ylimits,
            input$yticklbl,
            input$yaxislbl
          ),
          xrefline = c(input$rlxintercept, input$rlxcolor, input$rlxlinetyp),
          yrefline = c(input$rlyintercept, input$rlycolor, input$rlylinetyp)
        ))
      })
    }
    ft_out <- title_ftnote(
      domain = input$domain,
      summary_by = input$summary_by,
      filters = input$ae_filter,
      statistics = input$statistics,
      report = input$report
    )
    
    plots$plot_output <- out$plot
    if (input$report == "Forest") {
      plots$n <- out$n
      plots$drill <- out$drill_plt
    } else {
      plots$drill <- out$plot
    }
    plots$title <- ft_out[1]
    plots$ft <- ft_out[2]
    plots$plot_data <- out$data
    plots$ptly <- out$ptly
    output$plot_output <- renderPlotly({
      out$ptly
    })
    output$plot_UI <- renderUI({
      req(plots$plot_output)
      plotlyOutput("plot_output", width = "auto" , height = "auto") %>%
        withSpinner(type = 5)
    })
    output$title_UI <- renderText({
      req(plots$plot_output)
      ftnote <- ft_out[1]
      return(HTML(ftnote))
    })
    output$footnote_UI <- renderText({
      req(plots$plot_output)
      ftnote <- ft_out[2]
      return(HTML(ftnote))
    })
  })
  ### Definition of 'Obtain Statistics' button UI output -----------------------
  output$obtain_UI <- renderUI({
    req(data$data_in)
    div(style = 'height:6px;font-size: 5px;',
        actionBttn("obtain", "Create", color = "primary", style = 'simple'))
  })
  
  #List the details of filtered data that matches the selected SOC/PT
  output$plot_drill <- DT::renderDataTable({
    s = event_data("plotly_click", source = "plot_output")
    req(length(s) > 0)
    test <<- plots$drill$data[as.numeric(s$key), ]
    if (input$domain == "AE") {
      display <- analysis_data() %>%
        select(any_of(
          c(
            "USUBJID",
            "TRTVAR",
            "AEBODSYS",
            "AEDECOD",
            "AESER",
            "AEOUT",
            "AESTDT",
            "ASTTM",
            "AEENDT",
            "TRTSTDT",
            "TRTEDT",
            "TRTEMFL"
          )
        ))
      if (input$review_by == "PT") {
        plot_table = select(test, 'AEBODSYS', 'AEDECOD', 'TRTVAR') %>%
          inner_join(display)
      }
      if (input$review_by == "SOC") {
        plot_table = select(test, 'AEBODSYS', 'TRTVAR') %>%
          inner_join(display)
      }
    }
    if (input$domain == "LB") {
      display <- analysis_data() %>%
        select(any_of(
          c(
            "USUBJID",
            "TRTVAR",
            "PARAM",
            "PARAMCD",
            "AVAL",
            "ADT",
            "ANRIND",
            "ANRHI",
            "ANRLO",
            "AVISIT",
            "APBFL"
          )
        ), starts_with("ANL0"))
      plot_table = select(test, 'USUBJID', 'TRTVAR') %>%
        inner_join(display)
    }
    plot_table <- plot_table %>% distinct()
    datatable(
      plot_table,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = I('colvis'),
        pageLength = 10
      )
    )
  }, server = FALSE)
  
  output$dplot <- downloadHandler(
    filename = function() {
      paste0(
        ifelse(input$domain == "AE", input$report, input$report1),
        Sys.Date(),
        ".",
        input$fmt
      )
    },
    content = function(file) {
      title <-
        ggdraw() + draw_label(
          plots$title,
          size = 12,
          x = 0.05,
          hjust = 0,
          y = 0.5
        )
      foot <-
        ggdraw() + draw_label(
          plots$ft,
          size = 9,
          x = 0.05,
          hjust = 0,
          y = 0.5
        )
      pdfout <-
        plot_grid(
          title,
          plots$plot_output,
          foot,
          rel_heights = c(0.05, 0.9, 0.1),
          ncol = 1
        )
      #pdfout <- add_sub(pdfout1,plots$ft,y=0.5,x=0.05,hjust=0,size=10)
      if (input$fmt == "pdf") {
        #Save to pdf
        if (input$report == "Forest") {
          ploth <- max(15, ((plots$n) * 0.12) + 2)
          ggsave(
            file,
            pdfout,
            height = ploth,
            width = 15,
            device = "pdf",
            dpi = 300,
            units = "in",
            limitsize = F
          )
        }
        if (input$report %in% c("Volcano", "Edish")) {
          ggsave(
            file,
            pdfout,
            device = "pdf",
            height = 15,
            width = 15,
            dpi = 300,
            units = "in"
          )
        }
      }
      if (input$fmt %in% c("html", "pptx")) {
        tf1 <- tempfile(fileext = ".png")
        if (input$report == "Forest") {
          ploth <- max(10, ((plots$n) * 0.12) + 2)
          if (input$review_by == "SOC") {
            w = 15
          } else{
            w = 12
          }
          ggsave(
            tf1,
            pdfout,
            height = ploth,
            width = w,
            dpi = 100,
            units = "in"
          )
        }
        if (input$report == "Edish") {
          ggsave(
            tf1,
            pdfout,
            height = 10,
            width = 12,
            dpi = 100,
            units = "in"
          )
        }
        if (input$report == "Volcano") {
          ggsave(
            tf1,
            pdfout,
            height = 10,
            width = 15,
            dpi = 100,
            units = "in"
          )
        }
        if (input$fmt == "html") {
          txt <-
            RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")  # Convert the graphic image to a base 64 encoded string.
          myImage <-
            htmltools::HTML(sprintf('<img src="data:image/png;base64,%s">', txt))  # Save the image as a markdown-friendly html object.
          save_html(list(myImage), file)
        } else{
          read_pptx() %>% add_slide() %>%
            ph_with(external_img(tf1),
                    ph_location_fullsize(left = 0, top = 0)) %>%
            print(target = file)
        }
      }
    }
  )
}
