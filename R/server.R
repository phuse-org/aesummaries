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
      data$data_in <- data_processing(
        datain = input$analysis_data,
        data_source = input$source,
        server_path = input$server_path,
        data_filter = input$ae_filter,
        data_subset=input$subset,
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
      style = 'height:100px; width:91%; overflow: scroll',
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
      style = 'height:100px; width:91%; overflow: scroll',
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
              width = "75%")
  })
  
  output$treatment2_label_UI <- renderUI({
    req(data$data_in)
    req(input$summary_by != "Events")
    textInput("treatment2_label",
              "Label for Group B",
              value = "Treatment",
              width = "75%")
  })
  
  output$ctrlgrp_UI <- renderUI({
    div(
      style = 'height:100px; width:91%; overflow: scroll',
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
      style = 'height:100px; width:91%; overflow: scroll',
      checkboxGroupInput(
        "trtgrp",
        "Treatment Group",
        choices = setdiff(ARMCD(), input$ctrlgrp),
        selected = setdiff(ARMCD(), input$ctrlgrp)[1]
      )
    )
  })
  ### An essential step for obtaining statistics right after data uploading and before the generation of plot ------------------------------------
  observeEvent(input$obtain, {
    if (nrow(data$data_in)>0){
      if (input$period == "Other") {
        req(input$period_please_specify)
      }
      trtxx<-paste(input$trtgrp,collapse=",")
      print(input$report)
          withProgress(
            data$statistics <- GetStatistics_all(
              data = data$data_in,
              review_by = input$review_by,
              summary_by = input$summary_by,
              ctrlgrp = ifelse(input$report=='Forest',input$ctrlgrp,input$treatment1),
              trtgrp = ifelse(input$report=='Forest',trtxx,input$treatment2),
              statistics = input$statistics,
              alpha = input$alpha,
              cutoff = input$cutoff,
              sort_opt = input$sort_opt,
              sort_var=input$sort_by
            ),
            message = "Executing Get Statistics for EVENTS/ PT...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
      if (input$report == "Volcano") {
        withProgress(message = 'Generating Volcano Plot', value = 0, {
          data$out <- try(volcano_plot(
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
        })
      } else if (input$report == "Forest") {
        withProgress(message = 'Generating Forest Plot', value = 0, {
          data$out <- try(Forest_Plot(
            dat = isolate(data$statistics),
            review_by = input$review_by,
            summary_by = input$summary_by,
            statistics = input$statistics,
            xlims = c(0, 3),
            xref = as.numeric(input$X_ref),
            pvalcut= input$pvalcut
          ))
        })
        }
    ft_out <- title_ftnote(
      summary_by = input$summary_by,
      filters = input$ae_filter,
      statistics = input$statistics,
      report = input$report
    )
    
    plots$plot_output <- data$out$plot
    if (input$report == "Forest") {
      plots$n <- data$out$n
    } 
    plots$title <- ft_out[1]
    plots$ft <- ft_out[2]
    plots$ptly <- data$out$ptly
    output$plot_output <- renderPlotly({
      data$out$ptly
    })
    output$plot_UI <- renderUI({
      req(plots$plot_output)
      plotlyOutput("plot_output", width = "auto" ,height = "auto")%>%
        withSpinner(type = 5)
    })}
    
    #reactive statement - insert text in UI conditionally by using text plcaholder
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
  output$nodata <- reactive({
    req(data$data_in)
    return(nrow(data$data_in))
  })
  outputOptions(output, 'nodata', suspendWhenHidden = FALSE)
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
    if (input$report == "Forest") {
      test <<- data$out$drill_plt$data[as.numeric(s$key), ]
    } else {
      test <<- data$out$plot$data[as.numeric(s$key), ]
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
        plot_table = select(test, 'AEBODSYS', 'AEDECOD', 'TRTVAR') %>%
          inner_join(display)
      }
      if (input$review_by == "SOC") {
        plot_table = select(test, 'AEBODSYS', 'TRTVAR') %>%
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
        input$report,
        Sys.Date(),
        ".",
        ifelse(input$fmt=="Interactive","html",input$fmt)
      )
    },
    content = function(file) {
      if (input$fmt!="Interactive"){
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
      }else{
      saveWidget(plots$ptly,file=file)
    }}
  )
  output$version_info_UI <- renderText({
    verinfo<-sessionInfo()
    return(HTML(paste0("App devlopment software and packages details: "," \\n ",
                       sessionInfo()$R.version$version.string)
                ))
  })
  output$package_info_UI<-renderUI({x<-names(sessionInfo()$otherPkgs)
  return(lapply(1:length(x),
         function(i){
           c(paste0(x[i]," : ",packageVersion(x[i])))
         }))})
}
