#' @title xlsf_load
#' @description create xlsf object
#' @author Zack Arno
#' @param survey data.frame containing survey tab contents of xlsform
#' @param choices data.frame containing choices tab contents of xlsform
#' @param label colname that contians labels (default=label::English (en))
#' @param data list of data sets derived from form (main data set and any repeat group sets)
#' @TODO need to incorporate data arguments -- maybe ellipses for more than one argument
#' @export

xlsf_load <-  function(survey, choices,data_main=NULL,..., label="label::English (en)",sm_sep){
  xlsf <- list(survey= survey, choices= choices)
  xlsf <- purrr::map(.x = xlsf, ~.x %>% rename(`label`=label))
  if(!is.null(data_main)){
    xlsf <- c(xlsf , list(data_main = data_main,...))
  }
  class(xlsf)<- c(class(xlsf),"xlsf")
  attr(xlsf,"sm_sep")<- sm_sep
  return(xlsf)

}



