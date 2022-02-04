#' Merges variables into one separated by comma, the variables should have NA
#'
#' @param dataset_name input data set
#' @param var_name name of the variable that you need to merge into one
#'
#' @return
#' @export
#'
#' @examples fun_unite(dt, var_name = "var1")
func_unite <- function(dataset_name, var_name){

  var_name <- enquo(var_name)

  temp_data <- dataset_name %>%
    unite(!!var_name, contains(!!var_name), sep = ","
          , remove = TRUE, na.rm = TRUE)
  return(temp_data)
}
