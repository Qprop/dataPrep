#' Utility function to create variable label
#'
#' @param code_working manipulated subform containing all the ordered questions
#' @param question_type Question type for particular question
#' @param var_name Variable name of interest
#' @param loop_consideration whether there is any loop or not
#'
#' @return
#' @export
#'
#' @examples #func_var_lab(code_working, question_type, var_name, loop_consideration = FALSE)
func_var_lab <- function(code_working, question_type, var_name, loop_consideration = FALSE){

  if(!loop_consideration){
    return_var_lab <- code_working %>%
      filter(QType == question_type & QName == var_name) %>%
      mutate(var_lab = str_remove_all(English, regex("[0-9]+\\).*")),
             var_lab = str_remove_all(var_lab, "(\\\r\\\n)*"),
             var_lab = stri_enc_toutf8( var_lab, is_unknown_8bit = FALSE, validate = FALSE)) %>%
      pull(var_lab)
  }

  if(loop_consideration == TRUE){
    if(any(str_detect(code_working[["Programming Instructions"]], regex("loop", ignore_case = TRUE)) & code_working[["QName"]] == var_name & code_working[["QType"]] %in% c("Open Ended-Single Choice","Open Ended", "Single Choice"), na.rm = TRUE)){
      return_var_lab <- code_working %>%
        filter(QType == question_type & QName == var_name) %>%
        mutate(var_lab = str_remove_all(QName_loop, regex("[0-9]+\\).*")),
               var_lab = str_remove_all(var_lab, "(\\\r\\\n)*"),
               var_lab = stri_enc_toutf8( var_lab, is_unknown_8bit = FALSE, validate = FALSE)) %>%
        pull(var_lab)
    }
  }

  return(return_var_lab)
}
