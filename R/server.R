################################################################################
# server.R
# This R Script defines the server logics for the Shiny App.
################################################################################

server <- function(input, output, session) {
  data <- reactiveValues(
    domain = NULL,
    data_in = NULL,
    input_d=NULL,
    statistics = NULL,
    statistics_soc = NULL
  )

  plots <- reactiveValues(
    plot_data = NULL,
    plot_output = NULL,
    plot_click = NULL
  )

  ### Data uploading -----------------------------------------------------------
  analysis_data<-reactive({
    data$input_d<-data_read(datain = input$analysis_data)
  })
  
  # once data is loaded, all other inputs are loaded
  output$fileUploaded <-
    reactive({
      return(!is.null(analysis_data()))
    })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  ### Data processing ----------------------------------------------------------
  observe({
    req(data$input_d)
    fmq_list<-read.csv("FMQ_Consolidated_List.csv")
    withProgress(
      data$data_in <- data_processing(
        datain = data$input_d,
        Population_Filter = input$popfilter,
        data_filter = input$ae_filter,
        trtvar = input$trt_var,
        obs_period = input$period,
        obs_residual = input$period_please_specify,
        fmq_query_list = fmq_list
      ),
      message = "If reading data from server...",
      detail = "This step should take a while.",
      min = 0,
      max = 1,
      value = 1
    )
  })


  ### Extract all arms ---------------------------------------------------------
  ARMCD <- reactive({
    req(data$data_in)
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
      style = "height:100px; width:91%; overflow: scroll",
      radioButtons(
        "treatment1",
        "Group A",
        choices = ARMCD(),
        selected = ARMCD()[1],
        inline = FALSE
      )
    )
  })

  output$treatment2_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    div(
      style = "height:100px; width:91%; overflow: scroll",
      radioButtons(
        "treatment2",
        "Group B",
        choices = setdiff(ARMCD(), input$treatment1),
        selected = setdiff(ARMCD(), input$treatment1)[1],
        inline = FALSE
      )
    )
  })

  output$treatment1_label_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    textInput("treatment1_label",
      "Label for Group A",
      value = "Control",
      width = "75%"
    )
  })

  output$treatment2_label_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    textInput("treatment2_label",
      "Label for Group B",
      value = "Treatment",
      width = "75%"
    )
  })

  output$ctrlgrp_UI <- renderUI({
    div(
      style = "height:100px; width:91%; overflow: scroll",
      radioButtons(
        "ctrlgrp",
        "Control Group",
        choices = ARMCD(),
        selected = ARMCD()[1],
        inline = FALSE
      )
    )
  })

  output$trtgrp_UI <- renderUI({
    div(
      style = "height:100px; width:91%; overflow: scroll",
      checkboxGroupInput(
        "trtgrp",
        "Treatment Group",
        choices = setdiff(ARMCD(), input$ctrlgrp),
        selected = setdiff(ARMCD(), input$ctrlgrp)[1]
      )
    )
  })

  output$trtPairUI <- renderUI({
    req(input$ctrlgrp)
    req(input$trtgrp)
    pair <- c()
    for (i in input$trtgrp) {
      pair <- c(pair, paste0(input$ctrlgrp, " -vs- ", i))
    }
    div(
      style = "height:100px; width:91%; overflow: scroll",
      radioButtons(
        "trtpair",
        "Treatment Pair",
        choices = pair,
        selected = pair[1],
        inline = FALSE
      )
    )
  })

  output$query_list_UI <- renderUI({
    req(data$data_in)
    temp1 <- data$data_in
    temp2 <- ifelse(input$query_var == "SMQ", "smq_nam", "fmq_nam")
    query_list <- unlist(strsplit(unique(temp1[[temp2]]), "~~"))
    query_list1 <- unique(gsub("/\\w+", "", query_list[!is.na(query_list)]))
    div(
      style = "height:100px; width:91%",
      selectInput(
        "query_val",
        "Query Name",
        choices = query_list1,
        selected = query_list1[1]
      )
    )
  })

  output$PT_list_UI <- renderUI({
    req(data$data_in)
    req(input$query_val)
    req(input$query_cat)
    temp1 <- data$data_in
    temp2 <- ifelse(input$query_var == "SMQ", "smq_nam", "fmq_nam")
    temp3 <- if (toupper(input$query_cat) == "BROAD") c("BROAD", "NARROW") else "NARROW"
    pt <- temp1 %>%
      filter(str_detect(
        toupper(eval(parse(text = temp2))),
        toupper(paste(paste0(input$query_val, "/", temp3), collapse = "|"))
      )) %>%
      distinct(AEDECOD)
    pt_list <- unique(pt$AEDECOD)
    div(
      style = "height:100px; width:91%",
      selectInput(
        "pt_val",
        "Preferred Term",
        choices = pt_list,
        selected = pt_list[1]
      )
    )
  })

  ### An essential step for obtaining statistics right after data uploading and before the generation of plot ------------------------------------
  observeEvent(input$obtain, {
    if (nrow(data$data_in) > 0) {
      if (input$period == "Other") {
        req(input$period_please_specify)
      }
      withProgress(
        data$statistics <- GetStatistics_all(
          data = data$data_in,
          review_by = input$review_by,
          summary_by = input$summary_by,
          ctrlgrp = ifelse(input$report == "Forest", input$ctrlgrp, input$treatment1),
          trtgrp = ifelse(input$report == "Forest", paste(input$trtgrp, collapse = ","), input$treatment2),
          statistics = input$statistics,
          alpha = input$alpha,
          cutoff = input$cutoff,
          sort_opt = input$sort_opt,
          sort_var = input$sort_by
        ),
        message = "Executing Get Statistics for EVENTS/ PT...",
        detail = "This step should take a while.",
        min = 0,
        max = 1,
        value = 1
      )

      if (input$report == "Volcano") {
        withProgress(message = "Generating Volcano Plot", value = 0, {
          data$goutput <- try(volcano_plot(
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
            treatment2_label = input$treatment2_label,
            pvalcut = input$pvalcut
          ))

          data$toutput <- try(
            adaeT9(
              datain = data$data_in,
              population = input$popfilter,
              AE_Filter = input$ae_filter,
              ctrlgrp = input$treatment1,
              trtgrp = input$treatment2,
              ui_pctdisp = input$ui_pctdisp,
              ui_pt = input$ui_pt,
              ui_soc = input$ui_soc,
              ui_statistics = input$statistics,
              ui_trttotalyn = input$ui_trttotyn,
              ui_trtbign = input$ui_trtbign,
              ui_alpha = input$alpha,
              ui_cutoff = input$cutoff,
              ui_sortopt = input$sort_opt,
              ui_sortvar = input$sort_by
            )
          )
        })
      } else if (input$report == "Forest") {
        withProgress(message = "Generating Forest Plot", value = 0, {
          data$goutput <- try(Forest_Plot(
            dat = isolate(data$statistics),
            review_by = input$review_by,
            summary_by = input$summary_by,
            statistics = input$statistics,
            xlims = c(0, 3),
            xref = as.numeric(input$X_ref),
            pvalcut = input$pvalcut
          ))

          withProgress(message = "Generating Summary table", value = 0, {
            data$toutput <- try(
              adaeT9(
                datain = data$data_in,
                population = input$popfilter,
                AE_Filter = input$ae_filter,
                ctrlgrp = strsplit(input$trtpair, " -vs- ")[[1]][1],
                trtgrp = strsplit(input$trtpair, " -vs- ")[[1]][2],
                ui_pt = input$ui_pt,
                ui_soc = input$ui_soc,
                ui_pctdisp = input$ui_pctdisp,
                ui_statistics = input$statistics,
                ui_trttotalyn = input$ui_trttotyn,
                ui_trtbign = input$ui_trtbign,
                ui_alpha = input$alpha,
                ui_cutoff = input$cutoff,
                ui_sortopt = input$sort_opt,
                ui_sortvar = input$sort_by
              )
            )
          })
        })
      }

      ft_out <- title_ftnote(
        summary_by = input$summary_by,
        filters = input$ae_filter,
        statistics = input$statistics,
        report = input$report
      )

      plots$plot_output <- data$goutput$plot
      if (input$report == "Forest") {
        plots$n <- data$goutput$n
      }
      plots$title <- ft_out[1]
      plots$ft <- ft_out[2]
      plots$ptly <- data$goutput$ptly
      output$plot_output <- renderPlotly({
        data$goutput$ptly
      })

      output$plot_UI <- renderUI({
        req(plots$plot_output)
        plotlyOutput("plot_output", width = "auto", height = "auto") %>%
          withSpinner(type = 5)
      })

      output$summary_UI <- renderUI({
        req(data$toutput$tout)
        datatable <- data$toutput$tout
        htmltools_value(datatable)
      })

      output$gtitle_UI <- renderText({
        req(plots$plot_output)
        ftnote <- ft_out[1]
        return(HTML(ftnote))
      })
      output$gfootnote_UI <- renderText({
        req(plots$plot_output)
        ftnote <- ft_out[2]
        return(HTML(ftnote))
      })

      output$stitle_UI <- renderText({
        req(data$toutput$title)
        ftnote <- ft_out[1]
        return(HTML(ftnote))
      })

      output$sfootnote_UI <- renderText({
        req(data$toutput$footnote)
        ftnote <- ft_out[2]
        return(HTML(ftnote))
      })
    }
  })

  output$nodata <- reactive({
    req(data$data_in)
    return(nrow(data$data_in))
  })
  outputOptions(output, "nodata", suspendWhenHidden = FALSE)



  # List the details of filtered data that matches the selected SOC/PT
  output$plot_drill <- DT::renderDataTable(
    {
      s <- event_data("plotly_click", source = "plot_output")
      req(length(s) > 0)
      if (input$report == "Forest") {
        test <<- data$goutput$drill_plt$data[as.numeric(s$key), ]
      } else {
        test <<- data$goutput$plot$data[as.numeric(s$key), ]
      }
      display <- data$data_in %>%
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
        plot_table <- select(test, "AEBODSYS", "AEDECOD", "TRTVAR") %>%
          inner_join(display)
      }
      if (input$review_by == "SOC") {
        plot_table <- select(test, "AEBODSYS", "TRTVAR") %>%
          inner_join(display)
      }
      plot_table <- plot_table %>% distinct()
      datatable(
        plot_table,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = I("colvis"),
          pageLength = 10
        )
      )
    },
    server = FALSE
  )

  ##### Begin Event Analysis Process

  observe({
    req(data$data_in)
    req(input$query_val)
    req(input$pt_val)
    withProgress(message = "Generating Summary table", value = 0, {
      data$event_analysis_out <- try(
        event_analysis(
          datain = data$data_in,
          query_var = toupper(ifelse(input$query_var == "SMQ", "smq_nam", "fmq_nam")),
          query_val = toupper(input$query_val),
          query_scope = toupper(input$query_cat),
          pt_val = input$pt_val,
          ref_line = input$ref_line
        )
      )
      
      ft <- title_ftnote(
        summary_by = NULL,
        filters = NULL,
        statistics = NULL,
        report = "Event Analysis"
      )
      
    })

    ##### Begin Event Analysis Display

    output$event_plot_output1 <- renderPlotly({
      req(data$event_analysis_out$ptly)
      data$event_analysis_out$ptly
    })


    output$event1_UI <- renderUI({
      req(data$event_analysis_out$ptly)
      plotlyOutput("event_plot_output1", width = "auto", height = "auto")
    })
    
    output$etitle_UI <- renderText({
      req(data$event_analysis_out$ptly)
      return(HTML( ft[1]))
    })
    
    output$efootnote_UI <- renderText({
      req(data$event_analysis_out$ptly)
      return(HTML(ft[2]))
    })


    ##### End Event Analysis Display
  })


  output$dplot <- downloadHandler(
    filename = function() {
      paste0(
        input$report,
        Sys.Date(),
        ".",
        ifelse(input$fmt == "Interactive", "html", input$fmt)
      )
    },
    content = function(file) {
      if (input$fmt != "Interactive") {
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
        if (input$fmt == "pdf") {
          # Save to pdf
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
          if (input$report %in% c("Volcano")) {
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
              w <- 15
            } else {
              w <- 12
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
              RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt") # Convert the graphic image to a base 64 encoded string.
            myImage <-
              htmltools::HTML(sprintf('<img src="data:image/png;base64,%s">', txt)) # Save the image as a markdown-friendly html object.
            save_html(list(myImage), file)
          } else {
            read_pptx() %>%
              add_slide() %>%
              ph_with(
                external_img(tf1),
                ph_location_fullsize(left = 0, top = 0)
              ) %>%
              print(target = file)
          }
        }
      } else {
        saveWidget(plots$ptly, file = file)
      }
    }
  )
  output$version_info_UI <- renderText({
    verinfo <- sessionInfo()
    return(HTML(paste0(
      "App devlopment software and packages details: ", " \\n ",
      sessionInfo()$R.version$version.string
    )))
  })
  output$package_info_UI <- renderUI({
    x <- names(sessionInfo()$otherPkgs)
    return(lapply(
      1:length(x),
      function(i) {
        c(paste0(x[i], " : ", packageVersion(x[i])))
      }
    ))
  })
}
