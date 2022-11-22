

#' label_xml
#'
#' @param xlsf
#' @param q_name
#'
#' @return
#' @export
#'
#' @examples
label_vec_from_xml<- function(xlsf,q_name){
  dict<-xlsf_dict(xlsf)

  dict_filt<-dict[dict$name==q_name,] %>%
    unnest(c(choice_name, choice_label))


  recode_lookup <- dict_filt %>%
    pull(choice_label) %>%
    set_names(dict_filt %>% pull(choice_name))


  xlsf$data_main %>%
    mutate(!!q_name:=recode(!!sym(q_name),!!!recode_lookup)) %>%
    pull(q_name)


}


label_df_from_xml <- function(xlsf){

  df_names_not_empty<-xlsf$data_main %>%
    select(where(~!all(is.na(.)))) %>%
    colnames()

  df_names <- df_names_not_empty[df_names_not_empty %in% xlsf_get_so(xlsf)]

  df %>%
    dplyr::mutate(
      dplyr::across(df_names,~as.character(.x)),
      dplyr::across(df_names,~label_vec_from_xml(xlsf= xlsf,q_name=as.character(.x)))
    )
}
