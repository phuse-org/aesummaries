numbers_only <- function(x) !grepl("\\D", x)

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
    log_breaks(base = base),
    domain = c(1e-100, Inf)
  )
}

# Reading data
#' to read the input data loaded in the app
#'
#' @param datain input data browsed in the app
#'
#' @return returnes a datafram dsin
#' @export
#'
#' @examples
#' data_read(read_sas("../data/adae.sas7bdat"))

data_read <- function(datain) {
  if (is.null(datain)) {
    return()
  }
  if (file_ext(datain)[1] == "csv") {
    dsin <- read.csv(datain$datapath)
  }
  if (file_ext(datain)[1] == "sas7bdat") {
    dsin <- read_sas(datain$datapath)
  }
  if (file_ext(datain)[1] == "Rda") {
    dsin <- get(load(datain$datapath))
  }
  if (file_ext(datain)[1] == "xpt") {
    dsin <- read_xpt(datain$datapath)
  }
 print("data read success")
 return(dsin)
}



