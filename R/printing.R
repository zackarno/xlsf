#' print methods for xlsf object
#' special print for just data
#' print for just survey data
#' print for full objefct including data and questionnnaire

print.xlsf_data <-  function(df){

}
print.xlsf_survey <- function(df){

}


# data(xlsf_dat)
# data(dat)
# # debugonce(xlsf_load)
# xlsf_ob <-  xlsf_load(survey = xlsf[[2]],choices = xlsf[[1]], data_main=dat$hh_data,repeat1= dat$repeat_grp1,mynuts= dat$repeat_grp2,sm_sep = "/")
# xlsf_ob$mynuts
# print(xlsf_ob)
# # class(xlsf)

# structure(xlsf)
# print(xlsf)
# attributes(xlsf)


#' @export
print.xlsf <-  function(x){
  widgets <-  c("date","^int$","^integer$","select_multiple","select multiple","select_one","select one","text","time")

  if("data_main" %in% names(x)){
    num_respondents<-dim(x$data_main)[1]
    data_msg <-  glue::glue("data set attached with {crayon::green(num_respondents)} respondents")
  }else{
    data_msg <- "no data set attached"
  }

  cat("XLSForm survey composed of survey & choices sheet:",data_msg,sep = "\n")



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
  survey_type_n %>%
    print()
  if("data_main" %in% names(x)){
    x$data_main %>%
      print()
  }





}

