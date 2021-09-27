


#' need the list to be named
#' need label column to be chosen in teh creation of the xlsf object

# data(xlsf_dat)
# xlsf %<-% c("survey","choices")
#
# names(xlsf)<-c("choices","survey")
# xlsf <- map(.x = xlsf,.f = ~.x %>% rename(label="label::English (en)" ))



#' @title xlsf_dict
#' @description create nested look up table dictionary for xlsForm
#' @author Zack Arno
#' @param xlsf XLSForm entered as list of 2 named data frame (`survey`,`choices`)

xlsf_dict <- function(xlsf){

  survey_choices<-xlsf$choices %>%
    filter(!is.na(`list name`)) %>%
    group_by(`list name`) %>%
    summarise(
      choice_name= list(name),
      choice_label=list(label)
    )

  survey_questions<-xlsf$survey %>%
    select(type, name,label) %>%
    mutate(
      `list name`=  str_remove(type,"select_one|select one|select_multiple|select multiple") %>% trimws(),
      `type` = str_extract(type,"select_one|select one|select_multiple|select multiple|integer|int") %>% trimws()

    )

  survey_choices %>%
    left_join(survey_questions) %>%
    select(type, `list name`, everything()) %>%
    suppressMessages



}
