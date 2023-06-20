#' Utility function to handle var labelling to spss
#'
#' Part that does variable labelling
#'
#' @param var_name Variable name in focus
#' @param data_name Data set in focus
#' @param question_type Question type to be used
#'
#' @return
#' @export
#'
#' @examples #func_var_lab_spss(var_name, data_name, question_type)
func_var_lab_spss <- function(var_name, data_name, question_type) {
  ## Single Choice ----

  if (question_type == "Single Choice") {
    single_choice1 <- var_name %>% spss_func(trunc = TRUE)

    single_choice_avail_sub <-
      var_name[single_choice1 %in% colnames(data_name)]
    single_choice_avail_dt <-
      single_choice1[single_choice1 %in% colnames(data_name)]


    #Capture loop questions
    if (any(code_working[["QName"]] %in% var_name  &
            code_working[["QName_loop"]] %in% "")) {
      var_label <- code_working %>%
        func_var_lab(question_type = "Single Choice", var_name = var_name)
    }

    if (any(code_working[["QName"]] %in% var_name  &
            !code_working[["QName_loop"]] %in% "")) {
      var_label <- code_working %>%
        func_var_lab(
          question_type = "Single Choice",
          var_name = var_name,
          loop_consideration = TRUE
        )
    }

    #print(var_label)

    attr(data_name[[single_choice_avail_dt]], "label") <- var_label
  }

  ## Open ended single choice ----

  if (question_type == "Open Ended-Single Choice") {
    if (any(code_working[["QName"]] %in% var_name  &
            code_working[["QName_loop"]] %in% "")) {
      var_label <- code_working %>%
        func_var_lab(question_type = "Open Ended-Single Choice", var_name = var_name)
    }

    if (any(code_working[["QName"]] %in% var_name  &
            !code_working[["QName_loop"]] %in% "")) {
      var_label <- code_working %>%
        func_var_lab(
          question_type = "Open Ended-Single Choice",
          var_name = var_name,
          loop_consideration = TRUE
        )
    }



    if (var_name %in% colnames(data_name)) {
      attr(data_name[[var_name]], "label") <- var_label

      other_open_end_single_choice <- paste0(var_name, "_other")

      if (other_open_end_single_choice %in% colnames(data_name)) {
        attr(data_name[[other_open_end_single_choice]], "label") <-
          paste(var_label, "other")
      }
    }
  }

  ## Open Ended ------

  if (question_type %in% c("Open Ended", "Range")) {
    if (question_type == "Open Ended") {
      if (any(code_working3[["QName"]] %in% var_name  &
              code_working3[["QName_loop"]] %in% "")) {
        var_label <- code_working3 %>%
          func_var_lab(question_type = "Open Ended", var_name = var_name)
      }

      if (any(code_working3[["QName"]] %in% var_name  &
              !code_working3[["QName_loop"]] %in% "")) {
        var_label <- code_working3 %>%
          func_var_lab(question_type = "Open Ended",
            var_name = var_name,
            loop_consideration = TRUE
          )
      }




      if (spss_func(var_name) %in% colnames(data_name)) {
        attr(data_name[[spss_func(var_name)]], "label") <- var_label
      }
    }

    if (question_type == "Range") {
      (
        var_label <- code_working %>%
          func_var_lab(question_type = "Range", var_name = var_name)
      )
      if (spss_func(var_name) %in% colnames(data_name)) {
        attr(data_name[[spss_func(var_name)]], "label") <- var_label
      }
    }



  }

  ## SATA RATING -------

  if (question_type == "Select All That Apply Rating") {
    var_name1 <- data_name %>%
      select(contains(spss_func(var_name))) %>% colnames()

    (
      fct_value <- code_working %>%
        func_split_to_rows(question_type = "Select All That Apply Rating",
                           var_name = var_name, `Codes`) %>%
        mutate(label = stri_enc_toutf8(
          label, is_unknown_8bit = FALSE, validate = FALSE
        )) %>%
        mutate(var_names = str_c(var_name, "_", label)) %>%
        mutate(spss_names = spss_func(var_names))
    )

    for (j in seq_along(var_name1)) {
      attr(data_name[[var_name1[j]]], "label") <-
        unlist(fct_value[fct_value[["spss_names"]] == var_name1[j], "label"])
    }


  }

  ## SATA ------

  if (question_type == "Select All That Apply") {
    var_name1 <- data_name %>%
      select(contains(spss_func(var_name))) %>% colnames()

    (
      fct_value <- code_working %>%
        func_split_to_rows(question_type = "Select All That Apply",
                           var_name = var_name, `Codes`) %>%
        mutate(var_name = str_c(var_name, "_", label)) %>%
        mutate(spss_names = spss_func(var_name))
    )

    #Dealing with areas where naming is similar
    var_name1 <- var_name1[var_name1 %in% fct_value[["spss_names"]]]

    for (j in seq_along(var_name1)) {
      attr(data_name[[var_name1[j]]], "label") <-
        unlist(fct_value[fct_value[["spss_names"]] == var_name1[j], "label"])
    }


  }

  ## Matrix ----

  if (question_type == "MatrixTable") {
    var_name1 <- data_name %>%
      select(contains(var_name)) %>% colnames()


    (
      fct_value <- code_working %>%
        func_split_to_rows(question_type = "MatrixTable",
                           var_name = var_name, `Codes`) %>%
        mutate(label = stri_enc_toutf8(
          label, is_unknown_8bit = FALSE, validate = FALSE
        )) %>%
        mutate(var_names = str_c(var_name, "_", label)) %>%
        #Long variable name
        mutate(spss_names = spss_func(var_names))
    )

    (
      fct_value1 <- code_working %>%
        func_split_to_rows(question_type, var_name, `Matrix Columns`)
    )


    #Dealing with Checklist Matrix option

    if (any(str_detect(code_working[[which(code_working[["QName"]] == var_name), "Programming Instructions"]], "Checklist"), na.rm = TRUE)) {
      fct_value <-
        expand_grid(fct_value, brand = str_trim(fct_value1[["label"]], side = "both")) %>%
        tibble() %>%
        mutate(
          spss_names = str_c(spss_names, brand, sep = "_"),
          spss_names = spss_func(spss_names)
        )
    }


    var_name1 <- var_name1[var_name1 %in% fct_value[["spss_names"]]]

    for (j in seq_along(var_name1)) {
      attr(data_name[[var_name1[j]]], "label") <-
        unlist(fct_value[fct_value[["spss_names"]] == var_name1[j], "label"])
    }


  }


  ## Open Ended SATA ----

  if (question_type == "Open Ended-Select All That Apply") {
    Open_Ended_sata1 <- var_name %>%
      str_replace("'", "")

    var_name1 <- data_name %>%
      select(contains(spss_func(Open_Ended_sata1))) %>% colnames()

    (
      fct_value <- code_working %>%
        func_split_to_rows(question_type = "Open Ended-Select All That Apply",
                           var_name = var_name, `Codes`) %>%
        mutate(label = stri_enc_toutf8(
          label, is_unknown_8bit = FALSE, validate = FALSE
        )) %>%
        mutate(var_name = str_c(var_name, "_", label)) %>%
        mutate(spss_names = spss_func(var_name))
    )

    var_name1 <- var_name1[var_name1 %in% fct_value[["spss_names"]]]


    for (j in seq_along(var_name1)) {
      attr(data_name[[var_name1[j]]], "label") <-
        unlist(fct_value[fct_value[["spss_names"]] == var_name1[j], "label"])

      if (str_detect(var_name1[j], "_other$")) {
        other_open_end_sata <-
          var_name1[j][str_detect(var_name1[j], "_other$")]

        attr(data_name[[other_open_end_sata]], "label") <- "other"
        # }
      }
    }


  }

  return(data_name)
}
