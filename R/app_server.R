################################################################################
# app_server.R
# This R Script defines the server logic for the Shiny App.
################################################################################

############ Capture Reactive input values ###########
## Get the session Input element values

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  ReVal <- reactiveValues()
  observe({
    ReVal <- reactiveValuesToList(input)
  })

  ReVal <- reactiveValues(
    goutput = NULL,
    toutput = NULL
  )

  #################### Reading Data #################
  ## Read the input dataset from the source location
  output$readDataBtn_UI <- renderUI({
    if (input$source == "Default") {
      req(input$ADaM_Data_d)
    } else if (input$source == "Local") {
      req(input$analysis_data)
    }
    actionButton(
      "readData",
      "Read Data",
      color = "primary",
      style = "color: #fff; background-color: #337ab7;height: 25px;
                 border-color: #2e6da4;font-size: 12px;text-align: center;"
    )
  })

  observeEvent(input$readData,
    {
      print("reading data begin")
      if (input$source == "Default") {
        adam_data <- paste(input$ADaM_Data_d, collapse = ",")
      } else {
        adam_data <- input$analysis_data
      }
      withProgress(
        ReVal$adam <- data_read(
          ui_data_source = input$source,
          ui_adam_data = adam_data
        ),
        message = "Processing data reading step...",
        detail = "This step should take a while.",
        min = 0,
        max = 1,
        value = 1
      )
      removeUI("#readData")
      print("reading data success")
    },
    ignoreInit = TRUE
  )

  ## get the domain and analysis dataset
  observe({
    if (!is.null(ReVal$adam)) {
      print("Checking data read")
      temp1 <- ReVal$adam
      ReVal$sourcedata <- temp1$adam
      ReVal$domain <- names(temp1$adam)
      ReVal$data_attr <- temp1$adam_attrib
      test_domain <- names(temp1$adam)
      print("Checking data success")
    }
  })

  ## session output object to check if the Input data is not null

  output$dataUpload <- reactive({
    return(!is.null(ReVal$sourcedata))
  })
  outputOptions(output, "dataUpload", suspendWhenHidden = FALSE)

  ## To identify the base domain that is required for the report
  output$report_base_domain_UI <- renderUI({
    req(ReVal$sourcedata)
    selectInput("bdomain", "Report base domain", choices = ReVal$domain)
  })

  ####### Report Selection ####################
  ## Get the list of reports for option for a given domain
  observe({
    print("loading report meta begin")
    req(input$source)
    req(ReVal$sourcedata)
    ReVal$report_meta <- tibble::tribble(
      ~TA,
      ~DOMAIN,
      ~REPNAME,
      ~REPDESC,
      ~REPNO,
      ~REPTYPE,
      "Any",
      "ADAE",
      "adae_r001",
      "Summary of Adverse Events by System Organ Class and Preferred Term",
      "2.1",
      "Table",
      "Any",
      "ADAE",
      "Forest Plot",
      "Forest plot for adverse events",
      "2.2",
      "Figure",
      "Any",
      "ADAE",
      "Volcano Plot",
      "Volcano plot for adverse events",
      "2.4",
      "Figure",
      "Any",
      "ADAE",
      "Event Analysis",
      "Event analysis of MedRA query",
      "2.5",
      "Figure",
    )

    print("loading report meta success")
  })

  ## session output object to check domain split character
  output$report_meta <- reactive({
    return(!is.null(ReVal$report_meta))
  })
  outputOptions(output, "report_meta", suspendWhenHidden = FALSE)

  ## Create report selection input elements
  observe({
    req(ReVal$report_meta)
    req(input$bdomain)
    tempRepMeta <- ReVal$report_meta %>% filter(DOMAIN %in% c(toupper(input$bdomain)))
    output$report_TA_UI <- renderUI({
      req(tempRepMeta)
      selectInput("tarea", "Therapeutic Area", choices = pull(unique(tempRepMeta["TA"])))
    })
    output$report_Type_UI <- renderUI({
      req(tempRepMeta)
      req(input$tarea)
      selectInput("repType",
        "Report Type",
        choices = unique(tempRepMeta["REPTYPE"][tempRepMeta["TA"] == input$tarea])
      )
    })
    output$report_Nam_UI <- renderUI({
      req(input$repType)
      req(input$tarea)
      selectInput("repName",
        "Report Name",
        choices =
          unique(tempRepMeta["REPNAME"][tempRepMeta["TA"] == input$tarea &
            tempRepMeta["REPTYPE"] == input$repType])
      )
    })
    output$report_Num_UI <- renderUI({
      req(input$repType)
      req(input$repName)
      selectInput("repnum", "Report Number",
        choices = unique(tempRepMeta["REPNO"][tempRepMeta["REPNAME"] == input$repName])
      )
    })
    output$report_desc_UI <- renderUI({
      req(input$repType)
      req(input$repName)
      selectInput("repdesc", "Report Description",
        choices = unique(tempRepMeta["REPDESC"][tempRepMeta["REPNAME"] == input$repName])
      )
    })
  })

  output$repName <- reactive({
    return(tolower(input$repName))
  })
  outputOptions(output, "repName", suspendWhenHidden = FALSE)

  output$repType <- reactive({
    return(tolower(input$repType))
  })
  outputOptions(output, "repType", suspendWhenHidden = FALSE)
  ############## Treatment & Population Selection ############

  output$treatment_population_UI <- renderUI({
    req(ReVal$data_attr)
    req(input$repName)
    # Get list Treatment Variables as per CDISC ADaM IG
    trtv <-
      c(
        "ARM", "ARMCD", "ACTARM", "ACTARMCD", "STUDYID",
        "COHORT", "TRTA", "TRTP", "TRTAN", "TRTPN"
      )
    # Get the treatment variable from the input dataset
    trtvar <- ReVal$data_attr[[input$bdomain]] %>%
      filter(VAR_NAMES %in% trtv |
        str_detect(VAR_NAMES, "TRT[:digit:]+[AP]")) %>%
      select(VAR_NAMES) %>%
      pull()

    # Get the Population variable from the input dataset
    popvar <- ReVal$data_attr[[input$bdomain]] %>%
      filter(str_detect(tolower(VAR_LABEL), "population")) %>%
      mutate(pop = paste0(VAR_NAMES, " ~ ", VAR_LABEL)) %>%
      select(pop) %>%
      pull()

    # create select input element for Treatment Variable & Population variable
    fluidRow(
      column(
        width = 4,
        selectInput("trt_var",
          "Treatment Variable",
          choices = c(trtvar), selected = trtvar[2]
        )
      ),
      column(
        width = 4,
        selectInput("trt_sort",
          "Treatment Sort Variable",
          choices = c(trtvar), selected = trtvar[2]
        )
      ),
      column(
        width = 4,
        selectInput("popfilter", "Population Filter",
          choices = c("Overall Population", popvar)
        )
      )
    )
  })

  ################## Display Input Data #########################
  ## Get the list of variable from Input data for select input for basic data check
  output$source_data_varlist_UI <- renderUI({
    req(ReVal$sourcedata)
    varSelectInput("data_varlist",
      "Select Variables to be viewed",
      data = ReVal$sourcedata[[input$bdomain]],
      selected = NULL,
      multiple = TRUE
    )
  })

  ## Display the values of selected variables in the source data
  output$sourcedata_display <- DT::renderDataTable(
    {
      req(input$data_varlist)

      data_lkup <- ReVal$sourcedata[[input$bdomain]] %>%
        select(unlist(strsplit(paste(input$data_varlist, collapse = ","), ",")))

      datatable(
        data_lkup,
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

  ## Get the list of variable from Input data for select input for distinct value check

  output$categorical_variable_list_UI <- renderUI({
    req(ReVal$sourcedata)
    varSelectInput("categorical_variable",
      "Select Variable to see distinct values",
      data = ReVal$sourcedata[[input$bdomain]],
      selected = NULL
    )
  })

  ## Display the distinct values of the selected variables
  output$distinct_value_display <- DT::renderDataTable(
    {
      req(input$categorical_variable)
      if (length(input$categorical_variable) > 0) {
        distinct_value <-
          distinct(ReVal$sourcedata[[input$bdomain]], !!!input$categorical_variable)

        datatable(
          distinct_value,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = I("colvis"),
            pageLength = 10
          )
        )
      }
    },
    server = FALSE
  )


  ########## ADAE Analysis Module ###########
  output$AE_Event_Filter_UI <- renderUI({
    req(ReVal$sourcedata)
    req(input$repName)
    if (tolower(input$bdomain) == "adae") {
      print("AE filter UI field processing")
      if (tolower(input$repName) %in% c(
        "volcano plot", "forest plot",
        "adae_r001", "event analysis"
      )) {
        aeFil <- c(
          "Any",
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
      } else if (tolower(input$repName) %in% c("tornado plot")) {
        aeFil <- c(
          "Any Event",
          "Treatment Emergent",
          "Serious",
          "Drug-related",
          "Recovered/Resolved",
          "Recovering/Resolving",
          "Not Recovered/Not Resolved",
          "Fatal"
        )
      }
      selectInput("ae_filter",
        "Adverse Event Filter(s)",
        choices = aeFil,
        selected = aeFil[1],
        multiple = TRUE
      )
    }
  })

  output$aeCatVar_UI <- renderUI({
    req(ReVal$sourcedata)
    req(input$repName)
    req(ReVal$data_attr)
    if (tolower(input$bdomain) == "adae" &&
      tolower(input$repName) == "tornado plot") {
      print("AE categorical variable processing for Tornado plot")
      aCatV <- c("ASEV", "AESEV", "AETOXGR", "ATOXGR")
      # Get the treatment variable from the input dataset
      aeCatV <- ReVal$data_attr %>%
        filter(VAR_NAMES %in% aCatV) %>%
        select(VAR_NAMES) %>%
        pull()
      selectInput("aeCatVar",
        "AE Catagorical Variable",
        choices = aeCatV
      )
    }
  })

  # Get the variable list input for subgroup and By group selection for adae report
  observe({
    req(ReVal$sourcedata)
    req(input$repType)
    req(input$repName)
    req(input$bdomain)
    if (tolower(input$bdomain) == "adae" && input$repType == "Table") {
      print("AE Group variable and analysis variable processing")
      output$ae_subgrp_UI <- renderUI({
        varSelectInput(
          "aeSubGrpVar",
          "Sub Group Variables",
          data = ReVal$sourcedata[[input$bdomain]],
          selected = NULL,
          multiple = TRUE
        )
      })
      output$ae_byvar_UI <- renderUI({
        varSelectInput(
          "aeByVar",
          "By Group Variables",
          data = ReVal$sourcedata[[input$bdomain]],
          selected = NULL,
          multiple = TRUE
        )
      })
      output$ae_dptvar_UI <- renderUI({
        varSelectInput(
          "aeDptVar",
          "Dependent Variables",
          data = ReVal$sourcedata[[input$bdomain]],
          selected = NULL,
          multiple = TRUE
        )
      })

      output$pct_disp_UI <- renderUI({
        if (input$aeRiskYN == "N") {
          pct_denom <- c("Treatment" = "TRT", "Total" = "VAR", "High Term" = "HT")
        } else {
          pct_denom <- c("Treatment" = "TRT")
        }
        selectInput(
          inputId = "ui_pctdisp",
          label = "Percentage Denominator",
          choices = pct_denom
        )
      })
    }
  })

  observe({
    req(ReVal$sourcedata)
    req(input$bdomain)
    req(input$repName)
    req(input$trt_var)
    req(input$ae_filter)
    req(input$repType)
    if (tolower(input$bdomain) == "adae") {
      print("AE byVar processing start")
      ## evaluating the by variables based on report selection
      if (input$repType == "Table") {
        if (is.null(input$aeByVar)) {
          aeByV <- input$ae_hlt
        } else {
          aeByV <- c(paste(input$sl_byvar, collapse = ","), input$ae_hlt)
        }
      } else {
        if (tolower(input$repName) %in% c("volcano plot", "forest plot", "event analysis")) {
          aeByV <- input$ae_hlt
        } else {
          aeByV <- NA
        }
      }

      print("AE preprocessing start")
      ### calling Pre Processing AE data
      withProgress(
        ReVal$ae_pre <- ae_pre_processor(
          datain = ReVal$sourcedata[[input$bdomain]],
          ae_filter = input$ae_filter,
          aeSubset = ifelse(input$repType == "Table", input$aeSubset, NA),
          aeDenomSubset = ifelse(input$repType == "Table", input$aeDenomSubset, NA),
          aeObsPeriod = input$period,
          aeObsResidual = input$period_please_specify,
          trtvar = toupper(input$trt_var),
          trtsort = input$trt_sort,
          pop_fil = str_trim(unlist(strsplit(
            unique(input$popfilter), "~"
          ))[1]),
          fmq_data = utils::read.csv(paste0(
            app_sys("extdata"), "/FMQ_Consolidated_List.csv"
          )),
          aeEventVar = ifelse(is.null(input$ae_llt), input$ae_hlt, input$ae_llt),
          aeByVar = aeByV,
          aeSubGrpVar = ifelse(input$repType == "Table",
            ifelse(
              is.null(input$aeSubGrpVar),
              NA,
              paste(input$sl_subgrpvar, collapse = ",")
            ), NA
          ),
          aeBigN = ifelse(input$repType == "Table", input$aeBigN, "N"),
          aeGrpVarMiss = ifelse(input$repType == "Table", input$aeGrpVarMiss, "N"),
          aeTrtTot = ifelse(input$repType == "Table", input$aeTrtTot, "N"),
          aeSubGrpTot = ifelse(input$repType == "Table", input$aeSubGrpTot, "N")
        ),
        message = "Executing pre processing for EVENTS/ PT...",
        detail = "This step should take a while.",
        min = 0,
        max = 1,
        value = 1
      )
      print("AE preprocessing end")
    }
  })

  # deriving treatment pair for risk analysis

  observe({
    req(ReVal$ae_pre)
    req(input$repName)
    if (tolower(input$repName) %in% c("volcano plot", "forest plot") ||
      (tolower(input$repName) == "adae_r001" & input$aeRiskYN == "Y")) {
      print("AE treatment pair processing start")
      TRTCD <-
        reactive({
          return(unique(ReVal$ae_pre$dout$TRTVAR[ReVal$ae_pre$dout$TRTVAR != ""]))
        })

      ## Single pair radio button selection for Volcano plot

      output$treatment1_UI <- renderUI({
        radioButtons(
          "treatment1",
          "Control Group",
          choices = TRTCD(),
          selected = TRTCD()[1]
        )
      })

      output$treatment2_UI <- renderUI({
        radioButtons(
          "treatment2",
          "Treatment Group",
          choices = setdiff(TRTCD(), input$treatment1),
          selected = setdiff(TRTCD(), input$treatment1)[1]
        )
      })

      output$treatment1_label_UI <- renderUI({
        textInput("treatment1_label",
          "Label for Control Group",
          value = "Control"
        )
      })

      output$treatment2_label_UI <- renderUI({
        textInput("treatment2_label",
          "Label for Treatment Group",
          value = "Treatment"
        )
      })

      ## Multiple pair check box selection for forest plot

      output$ctrlgrp_UI <- renderUI({
        radioButtons(
          "ctrlgrp",
          "Control Group",
          choices = TRTCD(),
          selected = TRTCD()[1],
          inline = FALSE
        )
      })

      output$trtgrp_UI <- renderUI({
        checkboxGroupInput(
          "trtgrp",
          "Treatment Group",
          choices = setdiff(TRTCD(), input$ctrlgrp),
          selected = setdiff(TRTCD(), input$ctrlgrp)[1]
        )
      })
      print("AE treatment pair processing end")
    }

    output$aeSortBy_UI <- renderUI({
      req(input$repName)
      req(input$repName != "Event Analysis")

      if (tolower(input$repName) == "volcano plot") {
        print("")
      } else {
        if (tolower(input$repName) == "adae_r001" & input$aeRiskYN == "Y") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(input$repName) == "forest plot") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(input$repName) == "adae_r001" & input$aeRiskYN == "N") {
          by_var <- c("Count", "Percent")
        }
        selectInput(
          "sort_by",
          "Sorting Variable",
          choices = by_var
        )
      }
    })
  })

  ## event analysis related input fields
  observe({
    req(ReVal$ae_pre$dsin)
    req(input$repName)
    if (tolower(input$repName) %in% c("event analysis")) {
      print("AE event analysis hlt list input process start")
      output$aeHLT_list_UI <- renderUI({
        req(ReVal$ae_pre$dsin)
        req(input$ae_hlt)
        temp1 <- ReVal$ae_pre$dsin
        temp2 <- input$ae_hlt
        if (input$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
          hlt_list <- unlist(strsplit(unique(temp1[[temp2]]), "~~"))
          hlt_list1 <- sort(unique(gsub("/\\w+", "", hlt_list[!is.na(hlt_list)])))
        } else {
          hlt_list1 <- sort(unique(temp1[[temp2]]))
        }
        selectInput(
          "hlt_val",
          "Event Higher Classification",
          choices = hlt_list1,
          selected = hlt_list1[1]
        )
      })
      print("AE event analysis hlt list input process end")

      print("AE event analysis hlt scope input process start")
      output$aeHLT_query_cat_UI <- renderUI({
        req(input$ae_hlt)
        if (input$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
          selectInput("hlt_cat", "Query Scope", choices = c("Narrow", "Broad"))
        } else {
          print("")
        }
      })
      print("AE event analysis hlt scope input process end")

      print("AE event analysis llt list input process start")
      output$aeLLT_list_UI <- renderUI({
        req(ReVal$ae_pre$dsin)
        req(input$ae_hlt)
        req(input$hlt_val)
        if (input$ae_hlt %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
          req(input$hlt_cat)
        }
        temp1 <- ReVal$ae_pre$dsin
        temp2 <- input$ae_hlt

        if (input$ae_hlt %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
          if (toupper(input$hlt_cat) == "NARROW") {
            hl_val1 <- paste0(str_to_upper(input$hlt_val), "/", str_to_upper(input$hlt_cat))
          } else {
            hl_val1 <- input$hlt_val
          }
        } else {
          hl_val1 <- input$hlt_val
        }

        temp3 <- input$ae_llt

        llt_list <- unique(temp1[[temp3]][str_detect(toupper(temp1[[temp2]]), toupper(hl_val1))])

        if (input$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
          llt_list_ <- unlist(strsplit(unique(llt_list), "~~"))
          llt_list1 <- sort(unique(gsub("/\\w+", "", llt_list_[!is.na(llt_list_)])))
        } else {
          llt_list1 <- sort(llt_list)
        }

        selectInput(
          "llt_val",
          "Event Term",
          choices = llt_list1,
          selected = llt_list1[1]
        )
      })
      print("AE event analysis llt list input process end")

      print("AE event analysis llt scope input process start")
      output$aeLLT_query_cat_UI <- renderUI({
        req(input$ae_llt)
        if (input$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
          selectInput("llt_cat", "Query Scope", choices = c("Narrow", "Broad"))
        } else {
          print("")
        }
      })
      print("AE event analysis llt scope  input process end")
    }
  })


  # Generating risk values

  observeEvent(input$process, {
    req(input$repName)
    req(ReVal$ae_pre$dsin)
    req(ReVal$ae_pre$dout)
    req(input$statistics)
    req(input$ae_hlt)
    req(input$ae_llt)
    req(input$summary_by)
    if (tolower(input$repName) %in% c("volcano plot")) {
      req(input$treatment1)
      req(input$treatment2)
    }
    if (tolower(input$repName) %in% c("forest plot")) {
      req(input$ctrlgrp)
      req(input$trtgrp)
      req(input$sort_opt)
      req(input$sort_by)
    }
    req(input$alpha)
    req(input$cutoff)
    if (tolower(input$repName) %in% c("volcano plot", "forest plot")) {
      print("AE risk_stat process start")
      withProgress(
        ReVal$statistics <- risk_stat(
          datain = ReVal$ae_pre$dsin,
          d_datain = ReVal$ae_pre$dout,
          eventVar = ifelse(is.null(input$ae_llt), input$ae_hlt, input$ae_llt),
          summary_by = input$summary_by,
          ctrlgrp = ifelse(tolower(input$repName) == "volcano plot",
            input$treatment1,
            input$ctrlgrp
          ),
          trtgrp = ifelse(
            tolower(input$repName) == "volcano plot",
            input$treatment2,
            paste(input$trtgrp, collapse = "~~")
          ),
          statistics = input$statistics,
          alpha = input$alpha,
          cutoff = input$cutoff,
          sort_opt = ifelse(tolower(input$repName) == "forest plot", input$sort_opt, NA),
          sort_var = ifelse(tolower(input$repName) == "forest plot", input$sort_by, NA)
        ),
        message = "Executing Get Statistics for EVENTS/ PT...",
        detail = "This step should take a while.",
        min = 0,
        max = 1,
        value = 1
      )
      print("AE risk_stat process end")
    }
  })

  observeEvent(input$process, {
    req(input$repName)
    # generating Volcano plot

    if (tolower(input$repName) == "volcano plot") {
      req(ReVal$statistics)
      req(input$ae_filter)
      req(ReVal$statistics)
      req(input$statistics)
      req(input$treatment1)
      req(input$treatment2)
      req(input$X_ref)
      req(input$summary_by)
      req(input$pvalue_label)

      print("AE Volcano Plot process start")
      withProgress(message = "Generating Volcano Plot", value = 0, {
        ReVal$goutput <- try(volcano_plot(
          datain = isolate(ReVal$ae_pre$dsin),
          AE_Filter = input$ae_filter,
          statistics_data = isolate(ReVal$statistics),
          statistics = input$statistics,
          treatment1 = input$treatment1,
          treatment2 = input$treatment2,
          X_ref = as.numeric(input$X_ref),
          summary_by = input$summary_by,
          pvalue_label = input$pvalue_label,
          treatment1_label = input$treatment1_label,
          treatment2_label = input$treatment2_label,
          pvalcut = input$pvalcut
        ))
      })
      print("AE Volcano Plot process end")
    }

    # Generating Forest Plot

    if (tolower(input$repName) %in% c("forest plot")) {
      req(ReVal$statistics)
      req(input$ae_filter)
      req(input$ae_hlt)
      req(input$ae_llt)
      req(input$summary_by)
      req(input$statistics)
      req(input$pvalcut)
      req(input$X_ref)
      req(input$riskScale)
      print("AE Forest Plot process start")
      withProgress(message = "Generating Forest Plot", value = 0, {
        ReVal$goutput <- try(forest_plot(
          datain = isolate(ReVal$statistics),
          AE_Filter = input$ae_filter,
          review_by = c(input$ae_hlt, input$ae_llt),
          summary_by = input$summary_by,
          statistics = input$statistics,
          xref = as.numeric(input$X_ref),
          pvalcut = input$pvalcut,
          scale_trans = input$riskScale
        ))
      })
      print("AE Forest Plot process end")
    }
  })

  ## Generating Adae summary table
  observeEvent(input$process, {
    req(input$repName)
    if (tolower(input$repName) %in% c("adae_r001")) {
      req(ReVal$ae_pre)
      req(input$ae_filter)
      req(input$ae_llt)
      req(input$ae_hlt)
      req(input$summary_by)
      req(input$aeRiskYN)
      if (input$aeRiskYN == "Y") {
        req(input$treatment1)
        req(input$treatment2)
        req(input$statistics)
        req(input$alpha)
      }
      req(input$ui_pctdisp)
      req(input$cutoff)
      req(input$sort_opt)
      req(input$sort_by)
      print("AE Summary table process start")
      withProgress(message = "Generating AE Summary table", value = 0, {
        ReVal$toutput <- try(
          adae_r001(
            datain = ReVal$ae_pre,
            population = str_trim(unlist(strsplit(unique(input$popfilter), "~"))[1]),
            AE_Filter = input$ae_filter,
            riskyn = input$aeRiskYN,
            summary_by = input$summary_by,
            ctrlgrp = ifelse(input$aeRiskYN == "Y", input$treatment1, NA),
            trtgrp = ifelse(input$aeRiskYN == "Y", input$treatment2, NA),
            ui_lt = input$ae_llt,
            ui_ht = input$ae_hlt,
            ui_pctdisp = input$ui_pctdisp,
            ui_statistics = input$statistics,
            ui_trttotalyn = input$ui_trttotyn,
            ui_alpha = input$alpha,
            ui_cutoff = input$cutoff,
            ui_sortopt = input$sort_opt,
            ui_sortvar = input$sort_by
          )
        )
      })
      print("AE Summary table process end")
    }
  })

  ## Generate Event Analysis
  observe({
    req(ReVal$ae_pre)
    req(input$repName)
    req(input$ae_hlt)
    req(input$hlt_val)
    if (input$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
      req(input$hlt_cat)
    }
    req(input$ae_llt)
    req(input$llt_val)
    if (input$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
      req(input$llt_cat)
    }
    req(input$summary_by)
    if (tolower(input$repName) %in% c("event analysis")) {
      print("AE event analysis process start")
      withProgress(message = "Generating AE event analysis", value = 0, {
        ReVal$goutput <- try(event_analysis(
          datain = ReVal$ae_pre$dsin,
          datain_N = ReVal$ae_pre$dout,
          hl_var = input$ae_hlt,
          hl_val = toupper(input$hlt_val),
          hl_scope = toupper(input$hlt_cat),
          ll_var = input$ae_llt,
          ll_val = toupper(input$llt_val),
          ll_scope = toupper(input$llt_cat),
          summary_by = input$summary_by,
          ref_line = input$ref_line
        ))
      })
      print("AE event analysis process end")
    }
  })

  ############ Display Panel #############
  ## session output object to check if the graph is created
  output$g_output <- reactive({
    if (!is.null(ReVal$goutput)) {
      return(!is.null(ReVal$goutput))
    }
  })
  outputOptions(output, "g_output", suspendWhenHidden = FALSE)

  ## session output object to check if the table is created
  output$t_output <- reactive({
    if (!is.null(ReVal$toutput)) {
      return(!is.null(ReVal$toutput))
    }
  })
  outputOptions(output, "t_output", suspendWhenHidden = FALSE)

  ## display the interactive plot object on the dashboard area
  output$plot_output <- plotly::renderPlotly({
    ReVal$goutput$ptly
  })

  output$figure_UI <- renderUI({
    req(ReVal$goutput$ptly)
    div(
      plotly::plotlyOutput("plot_output", width = "auto", height = "auto") %>%
        shinycssloaders::withSpinner(type = 5),
      style = "overflow-x: scroll;"
    )
  })

  ## title for plot
  output$g_title_UI <- renderText({
    req(ReVal$goutput$ptly)
    rpt_title <- ReVal$goutput$title
    return(HTML(rpt_title))
  })

  ## footnote for Plot
  output$g_footnote_UI <- renderText({
    req(ReVal$goutput$ptly)
    rpt_ftnote <- ReVal$goutput$footnote
    return(HTML(rpt_ftnote))
  })

  ## display the summary table object on the dashboard area
  output$table_UI <- renderUI({
    req(ReVal$toutput)
    summary_out <- ReVal$toutput$tout
    div(style = "overflow-x: scroll;", htmltools_value(summary_out))
  })

  ## title for table
  output$t_title_UI <- renderText({
    req(ReVal$toutput)
    rpt_title <- ReVal$toutput$title
    return(HTML(rpt_title))
  })

  ## footnote for table
  output$t_footnote_UI <- renderText({
    req(ReVal$toutput)
    rpt_ftnote <- ReVal$toutput$footnote
    return(HTML(rpt_ftnote))
  })

  ############### Listing Generation ###########################
  ## creating listing for the data point selected in the interactive plot
  output$plot_listing <- DT::renderDataTable(
    {
      s <- plotly::event_data("plotly_click", source = "plot_output")
      req(length(s) > 0)

      ## getting the click event value key variables
      if (tolower(input$repName) == "forest plot" || tolower(input$repName) == "adae_r001") {
        test <- ReVal$goutput$drill_plt$data[as.numeric(s$key), ]
      } else {
        test <- ReVal$goutput$plot$data[as.numeric(s$key), ]
      }

      ## Getting Variables for AE plots and merging with key values to get selected records
      if (tolower(input$repName) == "forest plot" || tolower(input$repName) == "volcano plot" ||
        tolower(input$repName) == "adae_r001") {
        display <- ReVal$ae_pre$dout %>%
          select(any_of(
            c(
              "USUBJID",
              "TRTVAR",
              "BYVAR1",
              "DPTVAL",
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
        plot_table <- select(test, "BYVAR1", "DPTVAL", "TRTVAR") %>%
          inner_join(display) %>%
          rename(
            !!input$ae_hlt := "BYVAR1",
            !!input$ae_llt := "DPTVAL",
            !!input$trt_var := "TRTVAR"
          )
      }

      ## displaying the listing table

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

  ############### Save Reports ###########################

  ## getting the file formats selection values for graph, table and data frame
  output$save_fmt_ui <- renderUI({
    req(input$repType)
    if (input$repType == "Table") {
      save_fmt <- c("pdf", "html", "docx")
    } else if (input$repType == "Figure") {
      save_fmt <- c("pdf", "html", "docx", "pptx", "interactive")
    }
    radioButtons("save_fmt", "", choices = save_fmt)
  })

  ## generate the file name for the save file and dummy output if graph or table object is empty
  observe({
    req(input$repName)
    req(input$bdomain)
    req(input$save_fmt)
    ReVal$Save_Filename <- paste0(
      input$repName, "_",
      str_replace(input$repnum, "[[:punct:][:space:]]", "_"),
      str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
      ifelse(input$save_fmt == "interactive", "_I.html", paste0(".", input$save_fmt))
    )

    if (is.null(ReVal$goutput)) {
      ReVal$goutput$plot <- cowplot::ggdraw() + cowplot::draw_label(
        "Graph not created for this report, might be due to insufficient data",
        size = 9,
        x = 0.05,
        hjust = 0,
        y = 0.5
      )
      ReVal$goutput$title <- NULL
      ReVal$goutput$footnote <- NULL
      ReVal$goutput$rpt_data <- NULL
    }
    if (is.null(ReVal$toutput)) {
      ReVal$toutput$tout <-
        flextable(
          data.frame(Note = "Summary output not created for this report,
                     might be due to insufficient data"),
          cwidth = 5,
          cheight = 0.25
        )
      ReVal$toutput$title <- NULL
      ReVal$toutput$footnote <- NULL
      ReVal$toutput$rpt_data <- NULL
    }
    if (!is.null(ReVal$toutput) || !is.null(ReVal$goutput)) {
      if (input$repType == "Table") {
        ReVal$saveObj <- ReVal$toutput
      } else {
        ReVal$saveObj <- ReVal$goutput
      }
    }
  })

  ## Download the generated report if the data source is local or default
  output$save <- downloadHandler(
    filename = function() {
      ReVal$Save_Filename
    },
    content = function(file) {
      withProgress(
        save_file(
          save_object = ReVal$saveObj,
          file_format = input$save_fmt,
          report_type = input$repType,
          report_name = input$repName,
          file = file
        ),
        message = "Saving file...",
        detail = "This step should take a while.",
        min = 0,
        max = 1,
        value = 1
      )
    }
  )
}
