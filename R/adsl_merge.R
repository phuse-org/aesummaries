#' @title to merge the adsl dataset with the analysis dataset
#'
#' @param data adsl datset
#' @param pop population variable subset condition 
#' @param adsl_vars adsl variables to keep
#' @param dataset_add analysis dataset
#'
#' @return merged dataset
#' @export
#'
#' @examples
#' adsl_merge(data=adsl, 
#'            pop="SAFFL=='Y'",
#'            adsl_vars=c('USUBJID','TRT01A'), 
#'            dataset_add=data_in)
#'            
adsl_merge <- function(data=NULL, pop=NULL, dataset_add=NULL){
  
  pop <- toupper(pop)  
  
  if (!is.null(data) & is.null(pop)){
    adsl <- data 
  } else if (!is.null(data) & !is.null(pop)){
    adsl <- data %>% filter(eval(parse(text = toupper(pop))))
  } else {
    stop('no input data')
  }
  
  if (!is.null(data) & !is.null(dataset_add)){
    adsl_vars <- names(adsl)[which(!(names(adsl) %in% names(dataset_add)))]
    adsl <- adsl %>% select(USUBJID, all_of(adsl_vars))
  } else {
    stop('adsl_vars is required and variable names should match the dataset variable')
  }
  
  if (!is.null(data) & !is.null(dataset_add)){
    adsl_merge <- adsl %>%  
      left_join(dataset_add, by='USUBJID')
  } else if (is.null(data) | is.null(dataset_add)) {
    stop('no input data')
  }
  return(data.frame(adsl_merge))
}
