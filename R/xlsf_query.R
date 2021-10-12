
#' @title xlsf_query
#' @description quickly query throu kobo forms. Goal: minmize time you have to spend opening an checking XLSForm while processingn data in R
#' @author Zack Arno
#' @param xlsf XLSForm entered as list of 2 named data frame (`survey`,`choices`)
#' @param lab_query question label queryy
#' @import dplyr
#' @TODO when query returns more than  option, create interactivity to select correct
#' once connected to data set, it would be pretty cool to just give raw counts
#' @export


xlsf_query <- function(xlsf,pattern,.col="label"){
  dict <- xlsf_dict(xlsf)
  query_col_str <- switch(.col,
                      "xml_name"="name",
                      "label"="label")
  query_col_sym <- sym(query_col_str)

  dict_filt<-dict %>%
    filter(str_detect(!!query_col_sym,regex(pattern,ignore_case = T)))
  assertthat::assert_that(nrow(dict_filt)>0,
                          msg = "The pattern supplied provides no matches, please refine query.\nHint: in addition to querying the question label, you can also query coded xml_names, but you much switch the .col argument to xml_name")
    for(i in seq_len(nrow(dict_filt))){
    dict_loop <- dict_filt %>%
      slice(i)
    relevancy <- if_else(is.na(dict_loop$relevant),"none",dict_loop$relevant)

    cat(crayon::green("question_label:"), dict_loop$label,"\n")
    cat(crayon::green("xml_name:"), dict_loop$name,"\n")
    cat(crayon::green("relevancy:"), relevancy,crayon::green("    type:"),dict_loop$type)


    dict_loop %>%
      tidyr::unnest(c(choice_name, choice_label)) %>%
      select(choice_name, choice_label) %>%
      knitr::kable(
      ) %>% print()
    cat("\n\n")

  }


  }




# data(xlsf_dat)
#
# # xlsf_load just basically makes a named list of survye and choices
# xlsf <-  xlsf_load(survey = xlsf[[2]],choices = xlsf[[1]])
# # xlsf_query(xlsf,pattern =  "food")
# # xlsf_dict(xlsf)
# xlsf$survey %>%
#   filter(!is.na(relevant)) %>% select(label,relevant)
