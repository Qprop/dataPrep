func_var_lab <- function(code_working, question_type, var_name){
  code_working %>%
    filter(QType == question_type & QName == var_name) %>%
    mutate(var_lab = str_remove_all(English, regex("[0-9]+\\).*")),
           var_lab = str_remove_all(var_lab, "(\\\r\\\n)*"),
           var_lab = stri_enc_toutf8( var_lab, is_unknown_8bit = FALSE, validate = FALSE)) %>%
    pull(var_lab)
}
