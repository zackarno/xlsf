
#' @title xlsf_query
#' @description quickly query throu kobo forms. Goal: minmize time you have to spend opening an checking XLSForm while processingn data in R
#' @author Zack Arno
#' @param xlsf XLSForm entered as list of 2 named data frame (`survey`,`choices`)
#' @param lab_query question label queryy
#' `TODO` when query returns more than  option, create interactivity to select correct
#' once connected to data set, it would be pretty cool to just give raww counts


xlsf_query <- function(xlsf,lab_query){

  dict <- xlsf_dict(xlsf)
  dict_filt<-dict %>%
    filter(str_detect(label,lab_query))
  # if(nrow(dict_filt==0)){
  #   message("please refine query")
  # }
  if(nrow(dict_filt)==1){
    cat(crayon::green("xml_name:"), dict_filt$name,"\n")
    cat(crayon::green("question_label:"), dict_filt$label,"\n")
    cat(crayon::green("choices xml & labels:"),"\n")

    dict_filt %>%
      tidyr::unnest(c(choice_name, choice_label)) %>%
      select(choice_name, choice_label) %>%
      print()

  }



}
# xlsf_query(xlsf,lab_query = "Treating drinking water")
