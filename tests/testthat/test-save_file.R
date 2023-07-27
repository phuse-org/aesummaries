data(ae_pre)
forest_plot <- readRDS(paste0(app_sys("extdata"), "/", "forest_plot.rds"))

## 1. saving pdf output for forest plot

test_that("check for saving PDF file format of graph ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "pdf",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".pdf"
      )
    ), "saving the figure in PDF format passed"
  )
})

## 2. saving html figure output for forest plot

test_that("check for saving html file format of graph ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "html",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".html"
      )
    ), "generating figure output in html format passed"
  )
})

## 3. saving docx figure output for forest plot

test_that("check for saving DOCX file format of graph ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "docx",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".docx"
      )
    ), "generating figure output in DOCX format passed"
  )
})

## 4. saving pptx figure output for forest plot

test_that("check for saving PPTX file format of graph ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "pptx",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".pptx"
      )
    ), "generating figure output in PPTX format passed"
  )
})

## 5. saving pptx figure output for forest plot

test_that("check for saving interactive html file format of graph ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "interactive",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        "_I.html"
      )
    ), "generating figure output in interactive HTML format passes"
  )
})


## Saving table

## dummy Flex table
forest_plot$tout <- forest_plot$rpt_data %>% flextable()

## 6. saving html table output for dummy

test_that("check for saving html file format of table ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "html",
      report_name = "Forest Plot",
      report_type = "Table",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".html"
      )
    ), "generating table output in HTML/PDF/DOCX format passed"
  )
})

## 8. saving docx table output for dummy
test_that("check for saving docx file format of table ", {
  expect_message(
    save_file(
      save_object = forest_plot,
      file_format = "docs",
      report_name = "Forest Plot",
      report_type = "Table",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".docx"
      )
    ), "generating table output in HTML/PDF/DOCX format passed"
  )
})

## 9. check error for missing title or foot note
forest_plot1 <- list()
forest_plot1$plot <- forest_plot$plot
forest_plot1$ptly <- forest_plot$ptly
test_that("check for missing footnote or title ", {
  expect_error(
    save_file(
      save_object = forest_plot1,
      file_format = "pdf",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".pdf"
      )
    ), "Either footnote or title or not available, please check"
  )
})

## 10. check error for missing plot
forest_plot2 <- list()
forest_plot2$title <- forest_plot$title
forest_plot2$footnote <- forest_plot$footnote
forest_plot2$ptly <- forest_plot$ptly
test_that("check for missing plot", {
  expect_error(
    save_file(
      save_object = forest_plot2,
      file_format = "pdf",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".pdf"
      )
    ), "plot not available, please check"
  )
})

## 11. check error for missing title or foot note
forest_plot3 <- list()
forest_plot3$plot <- forest_plot$plot
forest_plot3$title <- forest_plot$title
forest_plot3$footnote <- forest_plot$footnote
test_that("check for missing plotly", {
  expect_error(
    save_file(
      save_object = forest_plot3,
      file_format = "interactive",
      report_name = "Forest Plot",
      report_type = "Figure",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        "_I.html"
      )
    ), "plotly not available, please check"
  )
})

## 12. check error for missing title or foot note
forest_plot4 <- list()
forest_plot4$title <- forest_plot$title
forest_plot4$footnote <- forest_plot$footnote
forest_plot4$ptly <- forest_plot$ptly
test_that("check for missing table", {
  expect_error(
    save_file(
      save_object = forest_plot4,
      file_format = "pdf",
      report_name = "Forest Plot",
      report_type = "Table",
      file = paste0(
        "ForestPlot", "_",
        str_replace("1.1", "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ".pdf"
      )
    ), "table not available, please check"
  )
})
