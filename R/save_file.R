#' To save the reports generated in the application
#'
#' @param save_object list of list containing, the Plot and Plotly, or flex table of the report
#' generated along with Tile and footnote
#' @param file_format the format in with file should be saved , possible values pdf, html, doc,
#' ppt, interactive html
#'  (`for Figures`) *Permitted Values*: `"pdf"`, `"html"`, `"docx"`, `"pptx"`, `"interactive"`
#'  (`for Tables`) *Permitted Values*: `"pdf"`, `"html"`, `"docx"`
#' @param report_type To identify whether the report type is table , figure is to be downloaded
#'    *Permitted Values*: `"table"`, `"figure"`
#' @param report_name Name of the report that is created formt he report meta list
#' @param file Name in which the report output file need to be saved
#'
#' @return auto save on click
#' @export
#'
#' @examples
#' \dontrun{
#' save_file (
#' save_object = <shiny reactive graph/table object>,
#' file_format = 'pdf',
#' report_name = "Forest Plot",
#' report_type = 'Figure',
#' file = <reportname, timestamp, format>
#' )
#' }

save_file <- function(save_object,
                      file_format,
                      report_type,
                      report_name,
                      file
                      ) {
  ##save object checks
  if (! "title" %in% names(save_object) || ! "footnote" %in% names(save_object)) {
    stop("Either footnote or title or not available, please check")
  }
  ## check if report type is figure
  if (str_detect(tolower(report_type), "figure")) {
    ##save object checks
    if (! "plot" %in% names(save_object)) {
      stop("plot not available, please check")
    }
    ## check if the format is not interactive
    if (tolower(file_format) != "interactive") {
      tryCatch(
        {
          ## get title info
          title <-
            cowplot::ggdraw() + cowplot::draw_label(
              save_object$title,
              size = 12,
              x = 0.05,
              hjust = 0,
              y = 0.5
            )
          ## get footnote details
          foot <-
            cowplot::ggdraw() + cowplot::draw_label(
              save_object$footnote,
              size = 9,
              x = 0.05,
              hjust = 0,
              y = 0.5
            )
          ## combine figure , title , footnote
          combine_out <-
            cowplot::plot_grid(
              title,
              save_object$plot,
              foot,
              rel_heights = c(0.05, 0.9, 0.1),
              ncol = 1
            )
          message("figure inital temporary output step passed")
        }
      )
      ## check if format requested is pdf
      if (file_format == "pdf") {
        tryCatch(
          {
            ggsave(
              file,
              plot = combine_out,
              height = ifelse(report_name == "Forest", max(15, ((save_object$n) * 0.12) + 2), 15),
              width = 15,
              device = "pdf",
              dpi = 300,
              units = "in",
              limitsize = FALSE
            )
            message("saving the figure in PDF format passed")
          }
        )
        ## if format requested is not pdf generate plot png temp file for plot
      } else {
        tryCatch(
          {
            tempfile <- tempfile(fileext = ".png")
            ggsave(
              tempfile,
              plot = combine_out,
              height = ifelse(report_name == "Forest", max(15, ((save_object$n) * 0.12) + 2), 15),
              width = 15,
              device = "png",
              dpi = 100,
              units = "in",
              limitsize = FALSE
            )
            message("generating temporary file for non PDF format passed")
          }
        )
        ## generate plot temp image file for html format
        if (file_format == "html") {
          tryCatch(
            {
              txt <- RCurl::base64Encode(
                readBin(tempfile, "raw", file.info(tempfile)[1, "size"]),
                "txt")
              # Convert the graphic image to a base 64 encoded string.
              myImage <- htmltools::HTML(sprintf('<img src="data:image/png;base64,%s">', txt))
              ## generate plot output in html format and download
              save_html(myImage, file = file)
              message("generating figure output in html format passed")
            }
          )
          ## generate plot output in pptx format and download
        } else if (file_format == "pptx") {
          tryCatch(
            {
              read_pptx() %>%
                add_slide() %>%
                ph_with(external_img(tempfile),
                        ph_location_fullsize(left = 0, top = 0)) %>%
                print(target = file)
              message("generating figure output in PPTX format passed")
            }
          )
          ## generate plot output in docx format and download
        }else if (file_format == "docx") {
          tryCatch(
            {
              read_docx() %>%
                body_add_img(src = external_img(tempfile),
                             width = 5, height = 8) %>%
                print(target = file)
              message("generating figure output in DOCX format passed")
            }
          )
        }
      }
      ## generate plotly output in html interactive format and download
    } else {
      if (! "ptly" %in% names(save_object)) {
        stop("plotly not available, please check")
      }
      tryCatch(
        {
          saveWidget(save_object$ptly,
                     file = file,
                     selfcontained = TRUE)
          message("generating figure output in interactive HTML format passes")
        }
      )
    }
  }
  ## check if report type is table
  if (str_detect(tolower(report_type), "table")) {
    if (!"tout" %in% names(save_object)) {
      stop("table not available, please check")
    }
    tryCatch(
      {
        if (file_format %in% c("pdf", "html", "docx")) {
          src <- normalizePath(paste0(app_sys("extdata"), "/", "save_table.Rmd"))
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, "save_table.Rmd", overwrite = TRUE)
          outf <- ifelse(file_format == "docx", "word_document",
                         paste0(file_format, "_document"))
          ## Render call to render the markdown in selected format
          ## Pass title, footnote and table object as input
          rmd_output <- rmarkdown::render("save_table.Rmd",
                                          output_format = outf,
                                          params = list(title = save_object$title,
                                                        ftnote = save_object$footnote,
                                                        ftable = save_object$tout))
          file.copy(rmd_output, file)
        }
        message("generating table output in HTML/PDF/DOCX format passed")
      }
    )
  }
}
