#' Utility Function to handle subform generation options
#'
#' @return
#' @export
#'
#' @examples #func_subform()

func_subform <- function(){

#Extract codes from the question
sub_form <- sub_form %>%
  mutate(Codes = str_extract(
    English,
    regex(
      "1\\)(.)*",
      multiline = TRUE,
      ignore_case = TRUE,
      dotall = TRUE
    )
  )) %>%
  mutate(English = str_remove(English, "'"))

# Renaming column
sub_form <- sub_form %>%
  dplyr::rename('QName' = 'Q Name',
                'QType' = 'Q Type',
                'QNumber' = 'Q #')

#Dealing with sub-from special characters adn Question Types
if(!any("Matrix Columns" == colnames(sub_form))){sub_form$`Matrix Columns`<- ""}
sub_form <- sub_form %>%
  mutate(Codes = str_replace_all(Codes, "\u2019", "'")) %>%
  mutate(Codes = stri_enc_toutf8(Codes, is_unknown_8bit = FALSE, validate = FALSE)) %>%
  mutate(English = str_replace_all(English, "\u2019", "'")) %>%
  mutate(English = str_replace(English, "amp;", "")) %>%
  mutate(English = str_replace(English, "&", "")) %>%
  mutate(English = str_replace(English, "Reply with .*", "")) %>%
  mutate(English = str_replace(English, "\\[OPERATOR.*", "")) %>%
  mutate(English = str_replace(English,"\\[ENUMERATOR*","")) %>%
  mutate(English = stri_enc_toutf8(English, is_unknown_8bit = FALSE, validate = FALSE)) %>%
  mutate(QType = str_replace(QType, "Matrix Table", "MatrixTable")) %>%
  mutate(QType = str_replace(QType, "GetLocation", "Open Ended")) %>%
  mutate(QType = str_replace(QType, "MobileNumber", "Open Ended")) %>%
  mutate(`Matrix Columns` = str_replace_all(`Matrix Columns`, "\u2019", "'"))


code_book <- sub_form %>%
  select('QNumber', 'QName','English','QType','Codes')


##Dealing with Loop & Merge, Matrix ----
var_list <-
  c(
    'QNumber',
    'QName',
    'English',
    'QType',
    'Codes',
    'Conditional',
    'Programming Instructions',
    'Skip Pattern',
    'Matrix Columns'
  )

code_instruction <- sub_form %>%
  select(any_of(var_list))


code_working <- code_instruction %>%
  mutate(matrix_fix = if_else(
    QType %in% c(
      "MatrixTable",
      "Matrix Table",
      "Open Ended-Select All That Apply",
      "Select All That Apply",
      "Select All That Apply Rating",
      "Range",
      "Open Ended-Single Choice",
      "Single Choice"
    ) &
      str_detect(`Programming Instructions`, "selected"),
    str_extract(`Programming Instructions`, '\\".*\\"'),
    ""
  )) %>%
  mutate(matrix_fix = str_remove_all(matrix_fix, '\\"'))

code_book_for_matrix <- code_instruction %>%
  select(QName, Codes) %>%
  rename(matrix_codes = "Codes") %>%
  filter(!is.na(matrix_codes))

code_working <- code_working %>%
  left_join(code_book_for_matrix, by = c("matrix_fix" = "QName"))

code_book_for_matrix_extra <- code_working %>%
  select(QName, matrix_fix, matrix_codes, Codes) %>%
  filter((matrix_fix != ""))

code_function_fill_multi_evoke <- function(data_codes){

  temp_list <- code_book_for_matrix_extra %>%
    filter(!is.na(matrix_codes)) %>%
    select(QName, matrix_codes)
  temp_codes <- data_codes %>%
    left_join(temp_list %>% rename(new_code = "matrix_codes"), by = c("matrix_fix" = "QName")) %>%
    mutate(matrix_codes = if_else(!is.na(new_code),
                                  new_code, matrix_codes)) %>%
    select(-new_code)

  return(temp_codes)

}


for(i in seq_along(code_book_for_matrix_extra$QName)){
  code_book_for_matrix_extra <- code_book_for_matrix_extra %>%
    code_function_fill_multi_evoke()
}

code_working <- code_working %>%
  left_join(code_book_for_matrix_extra %>%
              select(QName, matrix_codes), by = c("matrix_fix" = "QName"))

# code_working <- code_working %>%
#   mutate(matrix_codes = coalesce(matrix_codes.x, matrix_codes.y)) %>%
#   select(-c("matrix_codes.x","matrix_codes.y"))

code_working <- code_working %>%
  mutate(matrix_codes = case_when(
    is.na(matrix_codes.x) & !is.na(matrix_codes.y) ~ matrix_codes.y,
    !is.na(matrix_codes.x) & is.na(matrix_codes.y) ~ matrix_codes.x,
    !is.na(matrix_codes.x) & !is.na(matrix_codes.y) ~ paste0(matrix_codes.y,"\r\n",matrix_codes.x),
  ))


code_working <- code_working %>%
  select(-c(matrix_codes.y,matrix_codes.x))



code_working <- code_working %>%
  mutate(Codes = if_else(is.na(Codes),"",Codes)) %>%
  mutate(Codes = if_else(!is.na(matrix_codes) & QType != "MatrixTable",
                         paste0(matrix_codes,Codes),
                         Codes)) %>%
  mutate(Codes = if_else(!is.na(matrix_codes) & QType == "MatrixTable" &
                           !str_detect(`Programming Instructions`,"Enable Evoked Columns "),
                         paste0(matrix_codes,Codes),
                         Codes)) %>%
  mutate(Codes = str_trim(Codes, side = "both")) %>%
  mutate(`Matrix Columns` = if_else(!is.na(matrix_codes) & QType == "MatrixTable" &
                                      str_detect(`Programming Instructions`,"Enable Evoked Columns "),
                                    paste0(matrix_codes,`Matrix Columns`),
                                    `Matrix Columns`))

code_working <- code_working %>%
  mutate(loop_from = if_else(
    str_detect(`Programming Instructions`, "Loop"),
    str_extract(English, "#.*#"),
    ""
  )) %>%
  mutate(loop_from = str_remove_all(loop_from, "#")) %>%
  mutate(loop_from = if_else(is.na(loop_from), "", loop_from))

code_book_for_loop <- code_working %>%
  select(QName, Codes) %>%
  dplyr::rename(loop_Codes = "Codes") %>%
  filter(!is.na(loop_Codes))

code_working <- code_working %>%
  left_join(code_book_for_loop, by = c("loop_from" = "QName"))

#-------

code_working <- code_working %>%
  mutate(
    loop_Codes = str_replace_all(loop_Codes, "[0-9]+\\)", "|"),
    loop_Codes = str_replace(loop_Codes, "^\\|", "")
  ) %>%
  separate_rows(loop_Codes, sep = "\\|") %>%
  mutate(QName = if_else(!is.na(loop_Codes), str_c(QName, " (", loop_Codes, ")") , QName)) %>%
  mutate(QName_loop = if_else(
    !is.na(loop_Codes) &
      QType %in% c(
        "Open Ended",
        "Open Ended-Single Choice",
        "Single Choice",
        "Range",
        "Select All That Apply Rating",
        "Select All That Apply"
      ),
    loop_Codes,
    ""
  )) %>%
  select(-c("loop_Codes", "loop_from")) %>%
  mutate(QName = str_replace_all(QName, "\\\r\\n", ""))

code_working <- code_working %>%
  distinct(QName, .keep_all = TRUE) %>%
  mutate(QName = str_trim(QName, side = "both"))

### Exporting variabe list from subform -------

#return(code_working)

code_working1 <- code_working


# oe_sata <- code_working1 %>%
#   filter(QType == "Open Ended-Select All That Apply")
#
# oe_singel_choice <- code_working1 %>%
#   filter(QType == "Open Ended-Single Choice")

# matrix_singel_choice <- code_working1 %>%
#   filter(QType == "MatrixTable")



code_working1 <- code_working1 %>%
  mutate(open_ended_q = if_else(str_detect(QType, "Open Ended-"), "1,Other", "")) %>%
  separate_rows(open_ended_q, sep = ",") %>%
  mutate(QName = if_else(
    open_ended_q == "Other",
    paste0(QName, "_Other [specify]_other"),
    QName
  )) %>%
  mutate(English = if_else(open_ended_q == "Other", "Other", English)) %>%
  mutate(QType = if_else(open_ended_q == "Other", "Open Ended", QType))

code_working2 <- code_working1 %>%
  mutate(code_split = str_replace_all(Codes, "[0-9]+\\)", "`")) %>%
  mutate(value_num = str_remove(str_c(",", str_replace_all(Codes, "\\).*", "`")), "`$")) %>%
  mutate(value_num = str_replace(value_num, ",","`")) %>%
  mutate(code_split = if_else(
    QType %in% c(
      "Open Ended-Select All That Apply",
      "Select All That Apply",
      "Select All That Apply Rating",
      "MatrixTable"
    ),
    code_split,
    ""
  )) %>%
  separate_rows(code_split, value_num, sep = "`") %>%
  mutate(QName = if_else(code_split != "" &
                           QType %in% c(
                             "Open Ended-Select All That Apply",
                             "Select All That Apply",
                             "Select All That Apply Rating"
                           ),
                         paste0(QName, "_" , code_split), QName)) %>%
  mutate(QName = if_else(code_split != "" &
                           QType %in% c(
                             "MatrixTable"
                           ),
                         paste0(QName, ": " , code_split), QName)) %>%
  filter(!is.na(QName)) %>%
  filter(!(
    QType %in% c(
      "Open Ended-Select All That Apply",
      "Select All That Apply",
      "Select All That Apply Rating",
      "MatrixTable"
    ) & code_split == ""
  )) %>%
  mutate(code_split = "",
         value_num = "") %>%
  mutate(QName = str_replace_all(QName, "\\\r\\n", ""))


code_working2 <- code_working2 %>%
  mutate(code_split = if_else( QType %in% "MatrixTable" &
                                 str_detect(`Programming Instructions`,"Checklist"),
                               str_replace_all(`Matrix Columns`, "[0-9]+\\)", "`"),"")) %>%
  mutate(value_num = str_remove(str_c("`", str_replace_all(`Matrix Columns`, "\\).*", "`")), "`$")) %>%
  mutate(code_split = if_else(
    QType %in% "MatrixTable" &
      str_detect(`Programming Instructions`,"Checklist"),
    code_split,
    ""
  )) %>%
  separate_rows(code_split, sep = "`") %>%
  mutate(QName = if_else(code_split != "" &
                           QType %in% "MatrixTable" &
                           str_detect(`Programming Instructions`,"Checklist"),
                         paste0(QName, "_" , code_split), QName))  %>%
  filter(!(code_split == "" &  QType %in% "MatrixTable" &
             str_detect(`Programming Instructions`,"Checklist")))


code_working2 <- code_working2 %>%
  mutate(QName = str_remove_all(QName, "\\r\\n$")) %>%
  mutate(QName = str_remove_all(QName, "\\t$"))

code_working3 <- code_working2 %>%
  distinct(QName, .keep_all = TRUE)


code_list <- list("code_working" = code_working,
                  "code_working3" = code_working3)

return(code_list)
}
