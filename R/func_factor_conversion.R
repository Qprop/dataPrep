#' Utility function to convert to factors
#' Conversation Function
#'
#' @param var_name Variable name of interest
#' @param data_name Dataset to be used
#' @param question_type Question type of the variable
#'
#' @return
#' @export
#'
#' @examples #func_factor_conversion(var_name, data_name, question_type)
func_factor_conversion <- function(var_name , data_name, question_type) {

  #Creating a list of choice\answer options from subform options
  (fct_value <- code_working %>%
     func_split_to_rows(question_type, var_name, `Codes`))


  #Creating question labels

  (var_label <- code_working %>%
      func_var_lab(question_type, var_name))

  temp_data <- data_name  #Copy of working dataset

  var_name1 <-
    var_name   # Copy of variable name without unquoting and quosures


  if (var_name %in% colnames(temp_data)) {
    #Checking whether variable name exists in data

    var_name <-
      enquo(var_name)  #removing quotes and enabling use of !!

    ## Open Ended Single Choice ----

    if (question_type == "Open Ended-Single Choice") {
      #Handling Open ended single choice options

      other_var_name <- paste0(var_name1, "_Other [specify]_other")

      #print(other_var_name)

      #Working on other specify options

      temp_data[[other_var_name]] <-
        if_else(!temp_data[[var_name1]] %in% pull(fct_value, label),
                temp_data[[var_name1]], "")

      temp_data[[var_name1]][(temp_data[[var_name1]] != "") &
                               (!temp_data[[other_var_name]] %in% c("", "N/A"))] <-
        "Other [specify]"

      #Factor creation based on choice options | Currently only handling continuous numbering

      temp_data[[var_name1]] <-
        factor(temp_data[[var_name1]], levels = pull(fct_value, label))

      # temp_data <- temp_data %>%
      #   select(1:!!var_name, ncol(temp_data), everything())

    }

    ## Single Choice ----

    if (question_type == "Single Choice") {
      #Handling single choice options


      if (var_name1 %in% code_working[["QName"]])
      {
        temp_data[[var_name1]] <-
          factor(temp_data[[var_name1]], levels = pull(fct_value, label))

      }


    }

    ## SATA Rating ----

    if (question_type == "Select All That Apply Rating") {
      #Select All That Apply Rating

      (list_of_sata_var <-
         paste0(var_name1, "_", str_trim(fct_value[["label"]], side = "both")))
      #print(list_of_sata_var)

      for (i in seq_along(list_of_sata_var))

        temp_data[[list_of_sata_var[i]]] <-
          as.double(temp_data[[list_of_sata_var[i]]])


    }


  }

  ## Open Ended SATA ----

  if (question_type == "Open Ended-Select All That Apply") {

    (list_of_sata_var <-
       paste0(var_name1, "_", str_trim(fct_value[["label"]], side = "both")))
    #print(list_of_sata_var)

    for (i in seq_along(list_of_sata_var)) {
      if (list_of_sata_var[i] %in% colnames(temp_data)) {
        #print(list_of_sata_var[i])

        if (!str_detect(
          list_of_sata_var[i],
          regex("(Other \\[Speci)|(Others \\[Speci)|(Other \\[speci)|(Others \\[speci)", ignore_case = TRUE)
        )) {
          temp_data[[list_of_sata_var[i]]] <-
            factor(temp_data[[list_of_sata_var[i]]], levels = pull(fct_value, label))

        }

        if (str_detect(
          list_of_sata_var[i],
          regex("(Other \\[Speci)|(Others \\[Speci)|(Other \\[speci)|(Others \\[speci)", ignore_case = TRUE)
        )) {
          #new_var_name <- paste0(list_of_sata_var[i],"_recode")
          other_var_name <- paste0(list_of_sata_var[i], "_other")

          temp_data[[other_var_name]] <-
            if_else(!temp_data[[list_of_sata_var[i]]] %in% pull(fct_value, label),
                    temp_data[[list_of_sata_var[i]]],
                    "")


          temp_data[[list_of_sata_var[i]]][(temp_data[[list_of_sata_var[i]]] != "") &
                                             !(temp_data[[other_var_name]] %in% c("", "N/A"))] <-
            "Other [specify]"



          temp_data[[list_of_sata_var[i]]] <-
            factor(temp_data[[list_of_sata_var[i]]], levels = pull(fct_value, label))

        }

      }


      if (!list_of_sata_var[i] %in% colnames(temp_data)) {
        temp_data[list_of_sata_var[i]] <- as.character(NA)
        temp_data[list_of_sata_var[i]] <-
          factor(temp_data[[list_of_sata_var[i]]], levels = pull(fct_value, label))
      }


      if (!list_of_sata_var[str_detect(
        list_of_sata_var,
        regex("(Other \\[Speci)|(Others \\[Speci)|(Other \\[speci)|(Others \\[speci)", ignore_case = TRUE)
      )] %in% colnames(temp_data)) {
        other_add <-
          list_of_sata_var[str_detect(
            list_of_sata_var,
            regex("(Other \\[Speci)|(Others \\[Speci)|(Other \\[speci)|(Others \\[speci)", ignore_case = TRUE)
          )]

        temp_data[other_add] <- as.character(NA)

      }

    }

  }

  ## SATA ----

  if (question_type == "Select All That Apply") {
    (list_of_sata_var <-
       paste0(var_name1, "_", str_trim(fct_value[["label"]], side = "both")))
    #print(list_of_sata_var)

    for (i in seq_along(list_of_sata_var)) {
      if (list_of_sata_var[i] %in% colnames(temp_data)) {
        #print(list_of_sata_var[i])

        if (!str_detect(
          list_of_sata_var[i],
          regex("(Other \\[Speci)|(Others \\[Speci)|(Other \\[speci)|(Others \\[speci)", ignore_case = TRUE)
        )) {
          temp_data[[list_of_sata_var[i]]] <-
            factor(temp_data[[list_of_sata_var[i]]], levels = c("Yes", "No"))

        }

        if (str_detect(
          list_of_sata_var[i],
          regex("(Other \\[Speci)|(Others \\[Speci)|(Other \\[speci)|(Others \\[speci)", ignore_case = TRUE)
        )) {
          #new_var_name <- paste0(list_of_sata_var[i],"_recode")
          other_var_name <- paste0(list_of_sata_var[i], "_other")

          # temp_data[[other_var_name]] <-
          #   if_else(!temp_data[[list_of_sata_var[i]]] %in% pull(fct_value, label),
          #           temp_data[[list_of_sata_var[i]]],
          #           "")

          # temp_data[[list_of_sata_var[i]]][(temp_data[[list_of_sata_var[i]]] != "") &
          #                                    !(temp_data[[other_var_name]] %in% c("", "N/A"))] <-
          #   "Other [specify]"



          temp_data[[list_of_sata_var[i]]] <-
            factor(temp_data[[list_of_sata_var[i]]], levels = c("Yes", "No"))

          # temp_data <- temp_data %>%
          #   select(1:list_of_sata_var[i], ncol(temp_data), everything())

        }

      }

      if (!list_of_sata_var[i] %in% colnames(temp_data)) {

        temp_data[list_of_sata_var[i]] <- as.character(NA)
        temp_data[list_of_sata_var[i]] <-
          factor(temp_data[[list_of_sata_var[i]]], levels = c("Yes", "No"))

      }

    }


  }


  ## Matrix ----

  if (question_type == "MatrixTable") {

    (
      fct_value1 <- code_working %>%
        func_split_to_rows(question_type, var_name, `Matrix Columns`)
    )


    (list_of_sata_var <-
        paste0(var_name1, ": ", str_trim(fct_value[["label"]], side = "both")))

    #Dealing with Checklist Matrix option
    #any(str_detect(var_name, "^Brand"), na.rm = TRUE)
    if (any(str_detect(code_working[[which(code_working[["QName"]] == var_name), "Programming Instructions"]], "Checklist"), na.rm = TRUE)) {
      list_of_sata_var <-
        expand_grid(list_of_sata_var, brand = str_trim(fct_value1[["label"]], side = "both")) %>%
        tibble() %>%
        mutate(
          comb_d = str_c(list_of_sata_var, brand, sep = "_"),
          comb_d = str_replace(comb_d, "\u2019", "'")
        ) %>%
        pull(comb_d)
    }

    (
      matrix_value <- code_working %>%
        func_split_to_rows(question_type, var_name, `Matrix Columns`)
    )


    for (i in seq_along(list_of_sata_var)) {
      if (list_of_sata_var[i] %in% colnames(temp_data)) {
        #print(list_of_sata_var[i])

        temp_data[[list_of_sata_var[i]]] <-
          factor(temp_data[[list_of_sata_var[i]]], levels = pull(matrix_value, label))

      }


      if (!list_of_sata_var[i] %in% colnames(temp_data)) {
        #if(!str_detect(list_of_sata_var[i], regex("Other \\[Speci", ignore_case = TRUE))){
        temp_data[list_of_sata_var[i]] <- as.character(NA)
        temp_data[list_of_sata_var[i]] <-
          factor(temp_data[[list_of_sata_var[i]]], levels = pull(matrix_value, label))
        # }
      }
    }


  }

  return(temp_data)

}
