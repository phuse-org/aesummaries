#' ADSL
#'
#' Subject Level Analysis Dataset.
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/adsl.xpt>,
#' downloaded 2023-03-17
#' @format Data frame with 254 features and 58 fields
"adsl"

#' ADLB
#'
#' Laboratory Results Chemistry Analysis Dataset.
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/adlb.xpt>,
#' downloaded 2023-03-17
#'
#' @format Data frame with 74264 features and 46 fields
"adlb"

#' ADAE
#'
#' Adverse Events Analysis Dataset.
#'
#' @source <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/adae.xpt>,
#' downloaded 2023-03-17
#'
#' @format Data frame with 1191 features and 55 fields
"adae"

#' FMQ Consolidated List
#'
#' Consolidated list of FDA Medical Queries.
#'
#' @source
#' <https://github.com/phuse-org/aesummaries/tree/main/inst/extdata/FMQ_Consolidated_List.csv>,
#' downloaded 2023-03-17
#'
#' @format Data frame with 11712 features and 7 fields
"FMQ_Consolidated_List"

#' ae_pre
#'
#' Pre Processed `adae` data from `ae_pre_processor()`
#'
#' @format List of data frames of length `3`
"ae_pre"

#' ae_risk
#'
#' Output from `risk_stat()`
#'
#' @format Data frame with `46` rows and `17` variables
"ae_risk"

#' event_df
#'
#' Output from `risk_stat()` required for `event_analysis()`
#'
#' @format List containing 2 data frames `dsin` and `dout` (to be passed in `event_analysis`) and
#' `bigN` value
"event_df"
