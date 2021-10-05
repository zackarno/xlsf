#' print methods for xlsf object
#' special print for just data
#' print for just survey data
#' print for full objefct including data and questionnnaire

print.xlsf_data <-  function(df){

}
print.xlsf_survey <- function(df){

}

print.xlsf <-  function(df){



}

# data(xlsf_dat)
# debugonce(xlsf_load)
# xlsf <-  xlsf_load(survey = xlsf[[2]],choices = xlsf[[1]],sm_sep = "/")
# class(xlsf)

# structure(xlsf)
# print(xlsf)
# attributes(xlsf)

#' @export
print.xlsf <-  function(x){
  widgets <-  c("date","^int$","^integer$","select_multiple","select multiple","select_one","select one","text","time")

  data_msg <-  ifelse("data"%in% names(x),"data set attached","no data set attached")
  cat("XLSForm survey compposed of survey & choices sheet:",data_msg,sep = "\n")


  survey_type_n <-  x$survey %>%
    filter(!stringr::str_detect(type," group$|_group$|_repeat$|^note$|^calculate$")) %>%
    mutate(type= stringr::str_extract(type, glue::glue_collapse(x = widgets,sep = "|") )) %>%
    mutate(type= case_when(
      type %in% c("int","integer")~ "numeric integer",
      type %in%  c("select_multiple","select multiple")~ "select_multiple",
      type %in%c("select_one","select one")~ "select_one",
      type %in%c("text")~ "free text",
      type %in%c("time")~ "time",
      type %in%c("date")~ "date",
      TRUE~type
    )) %>%
    count(type)
  survey_type_n




}
print(xlsf)
