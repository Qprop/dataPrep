#' SPSS Variable naming convention change
#'
#' Removing special characters
#' @param var_naming variable name in focus
#' @param trunc whether we are truncating the characters to 64
#'
#' @return
#' @export
#'
#' @examples #spss_func(var_naming, trunc = TRUE)
spss_func <- function(var_naming, trunc = TRUE) {
  var_hold <- var_naming %>%
    str_remove_all("'|\\/|\\s|\\&") %>%
    str_replace_all("\\[", "_") %>%
    str_remove("\\]") %>%
    str_replace_all("\\-", "_") %>%
    str_remove_all("'") %>%
    str_remove_all("\\u2019") %>%
    str_remove_all("\\+") %>%
    str_remove_all("amp;") %>%
    str_replace_all("\\:", "_") %>%
    str_replace_all("\\(|\\)", "_") %>%
    str_remove("_$") %>%
    str_remove_all(",")

  if (trunc) {
    var_hold <- var_hold %>%
      str_trunc(width = 64,
                side = "right",
                ellipsis = "")
  }
  return(var_hold)
}
