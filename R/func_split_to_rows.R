#' Title
#'
#' @param dt_name data set
#' @param question_type question type e.g. Single Choice
#' @param var_name variable name in subform
#' @param sub_form_column sub form column you are picking from `matrix codes` or `codes`
#'
#' @return
#' @export
#'
#' @examples # func_split_to_rows(dt_name = code_working,
#                    var_name = "ConsumedP24H_2",
#                    question_type = "MatrixTable",
#                    sub_form_column = `Matrix Columns`)
func_split_to_rows <-
  function(dt_name,
           question_type,
           var_name,
           sub_form_column) {

    sub_form_column <- enquo(sub_form_column)
    question_type <- enquo(question_type)
    var_name <- enquo(var_name)
    dt_name %>%
      filter(QType == !!question_type & QName == !!var_name) %>%
      mutate(value_recode = str_replace_all(!!sub_form_column, "[0-9]+\\)", ",")) %>%
      select(value_recode) %>%
      separate_rows(value_recode, sep = ",") %>%
      mutate(value_recode = str_remove_all(value_recode, "\r\n")) %>%
      distinct(value_recode) %>%
      filter(value_recode != "") %>%
      tibble::rowid_to_column(var = "value") %>%
      rename(label = "value_recode") %>%
      mutate(label = str_trim(label, side = "right"))

     }

