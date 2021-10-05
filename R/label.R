#' label xlsForm survey dataest
#' @param xlsf xlsf object


xlsf_label <- function(xlsf){
  xlsf <- xlsf_label_q(xlsf)
  xlsf <-  xlsf_label_r(xlsf)
  }


#' label colnames
xlsf_label_q <- function(xlsf){

}

#' label dataframe contents
xlsf_label_r <- function(xlsf){

}


#' mutate labels based on 1 column containing question name and other containing question value
#' typically used to label long format analyzed data

xlsf_mutate_label <- function(df, q_names, q_values){


}



