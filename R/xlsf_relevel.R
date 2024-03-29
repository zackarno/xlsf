

#' @title xlsf_relevel
#' @description relevel data to XLSForm. fct_expand applied to relevel all select questions to choices in questionnaire. all select_multiple converted to logical
#' @author Zack Arno
#' @param df XLSForm data set (i.e kobo/ODK)
#' @param xlsf xlsf class object
#' @param skip column to skip in releveling (default = NULL)

#' @TODO research if i should use sm_sep attribute from xlsf_load rather than named argument
#'
#' @export



xlsf_relevel<- function(df, xlsf,skip=NULL, sm_sep="/"){

  so_names <- intersect(xlsf_get_so(xlsf), colnames(df))
  sm_names <- intersect(xlsf_get_sm(xlsf), colnames(df))

  if(!is.null(skip)){
    skip_rgx <- glue::glue_collapse(x = glue::glue("^{skip}$"),sep = "|")
    so_names <- so_names[!str_detect(so_names,skip_rgx)]
    sm_names <- sm_names[!str_detect(sm_names,skip_rgx)]
  }
  cat(crayon::green("factorzing select ones & making all select multiple logical:\n"))
  df %>%
    mutate(
      across(all_of(so_names),~as.character(.x)),
      across(all_of(so_names),~xlsf_fct_so(.,.x,xlsf)) ,
      across(all_of(sm_names),~as.logical(.x))
    )

}
#' @title xlsf_fct_so
#' @description relevel select ones to questionnaire options. Designed for use in dplyr::across
#' @author Zack Arno
#' @param df XLSForm data set (i.e kobo/ODK)
#' @param q_name question to be refactored
#' @param xlsf object of class xlsf

#' @TODO research if i should use sm_sep attribute from xlsf_load rather than named argument
#'


xlsf_fct_so<- function(df,q_name,xlsf){
  dict<-xlsf_dict(xlsf)
  so_dict <- dict %>%
    filter(str_detect(type,"^select_one|^select one")) %>%
    unnest(c(choice_name,choice_label))


  q_name <- cur_column()
  cat(q_name,"\n")
  so_q_dict<- so_dict %>%
    filter(name==q_name)

  choice_lookup <- so_q_dict %>%
    dplyr::pull(choice_name)

  df_expanded <- forcats::fct_expand(forcats::as_factor(df), choice_lookup)

  forcats::fct_relevel(df_expanded, choice_lookup)


}
#' @title xlsf_get_sm
#' @description retrieve column names of all select one questions in questionnaire
#' @author Zack Arno
#' @param xlsf object of class= xlsf
#' @param sm_sep character used to sepearate select multiple question/choices (default=/)
#' @TODO research if i should use sm_sep attribute from xlsf_load rather than named argument
#'

xlsf_get_sm<- function(xlsf=kobo,sm_sep="/"){
  dict<-xlsf_dict(xlsf)
  sm_dict <- dict %>%
    filter(str_detect(type,"^select_multiple|^select multiple")) %>%
    unnest(c(choice_name,choice_label)) %>%
    mutate(xml_analysis_name= glue::glue("{name}{sm_sep}{choice_name}"))
  #needa add this.
  sm_dict %>%
    pull(xml_analysis_name)


}
xlsf_get_so<- function(xlsf){
  dict<-xlsf_dict(xlsf)
  dict %>%
    filter(str_detect(type,"^select_one|^select one")) %>%
    pull(name)

}


