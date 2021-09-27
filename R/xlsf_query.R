
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
      knitr::kable(label = "here it is")

  }



}

# # xlsf <-  xlsf_load(survey = xlsf[[2]],choices = xlsf[[1]])
# xlsf_query(xlsf,lab_query = "Treating drinking water")
# dict_filt<-dict %>%
#   filter(str_detect(label,"water"))
#
# print_these_tables<- dict_filt %>%
#   tidyr::unnest(c(choice_name, choice_label)) %>%
#   select(name,choice_name, choice_label) %>%
#   split(.$name)
#
# library(knitr)
# library(kableExtra)
# ## tables
# for(i in seq_along(print_these_tables)) {
#
#     kable(print_these_tables[[i]],  caption = names(print_these_tables)[i], longtable = TRUE) #%>%
#       # kable_styling(font_size = 7, latex_options = "repeat_header", full_width = FALSE)
# }
# options(kableExtra_view_html = F)
# options(kableExtra_view_html = F)
# print_these_tables[[1]] %>%
#   knitr::kable(caption = 'Two tables created by knitr::kables().')
# tables <- imap(print_these_tables, ~ kable(.x, caption = .y))
