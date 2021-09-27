#' @title xlsf_load
#' @description create xlsf object
#' @author Zack Arno
#' @param survey data.frame containing survey tab contents of xlsform
#' @param choices data.frame containing choices tab contents of xlsform
#' @param label colname that contians labels (default=label::English (en))
#' @param data list of data sets derived from form (main data set and any repeat group sets)
#' `TODO` need to incorporate data arguments -- maybe ellipses for more than one argument

xlsf_load <-  function(survey, choices, label="label::English (en)"){
  xlsf <- list(survey= survey, choices= choices)
  purrr::map(.x = xlsf, ~.x %>% rename(`label`=label))
}




