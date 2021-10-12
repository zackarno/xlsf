
# extract_data <- function(xlsf){
#   purrr::discard(xlsf %>% unclass(),.p = ~stringr::str_detect(.x,"survey"))
#   xlsf %>% unclass() %>%
#     discard(.p = ~stringr::str_detect(.x,"survey|choices"))
#   xlsf %>%unclass() %>%  list_modify("survey|choices"=NULL)
#   xlsf[[
#   names(xlsf)!="survey"
#   ]]
#   xlsf %>% tidyselect:::select(-matches("survey|choices"))
#     keep(~names(.x)!= c("survey","choices"))
#
# }
label_xml<- function(xlsf,q_name){
  dict<-xlsf_dict(xlsf)
  # cur_
  # q_name <- cur_column()
  # so_q_dict<-dict %>%
  #   filter(name==q_name) %>%
  #   unnest(choice_name,choice_labels)
  dict_filt<-dict[dict$name==q_name,] %>%
    unnest(c(choice_name, choice_label))


  recode_lookup <- dict_filt %>%
    pull(choice_label) %>%
    set_names(so_q_dict %>% pull(choice_name))

  # recode(df,!!!recode_lookup)
  xlsf$data_main %>%
    mutate(!!q_name:=recode(!!sym(q_name),!!!recode_lookup)) %>%
    pull(q_name)
  #
  # df %>%
  #   mutate(!!q_name:=recode(!!sym(q_name), !!!recode_lookup)) %>%
  #   pull(q_name)
}


label_choices<- function(df, kc, ks){
  dicts<-make_xlsf_dicts(kc = kc,ks =ks)
  df_names_not_empty<-df %>%
    select(where(~!all(is.na(.)))) %>% colnames()
  df_names <- df_names_not_empty[df_names_not_empty %in% dicts$so_dict$name]

  df %>%
    mutate(
      across(df_names,~as.character(.x)),
      across(df_names,~label_xml(.,q_name=as.character(.x),kc=kc,ks=ks))
    )
}
