### BMI to groups

grp_bmi <- function(x, depth ='full', style = 'name') { ## x is numeric BMI variable ## type is 'full' [default] or 'part' BMI groups
  # check input type, error message
  if(!is.numeric(x)) {
    stop("BMI input value must be numeric", call. = FALSE)
  }
  # check output group detail
  else if(depth == 'full' & style == 'name') {
    # main function
    output <- factor(dplyr::if_else(x <18.5, "Underweight",
                                    dplyr::if_else(x >=18.5 & x <=25, "Healthy Weight",
                                                   dplyr::if_else(x >25 & x <30, "Overweight",
                                                                  dplyr::if_else(x >=30 & x <35, "Obesity Stage I",
                                                                                 dplyr::if_else(x >=35 & x <40, "Obesity Stage II",
                                                                                                dplyr::if_else(x >=40, "Obesity Stage III", NA_character_
                                                                                                )))))), ordered = TRUE, levels = c( "Underweight", "Healthy Weight", "Overweight", "Obesity Stage I", "Obesity Stage II", "Obesity Stage III"))
  }
  # check output group detail
  else if(depth == 'part' & style == 'name') {
    # main function
    output <- factor(dplyr::if_else(x <18.5, "Underweight",
                                    dplyr::if_else(x >=18.5 & x <=25, "Healthy Weight",
                                                   dplyr::if_else(x >25 & x <30, "Overweight",
                                                                  dplyr::if_else(x >=30, "Obese", NA_character_
                                                                  )))), ordered = TRUE, levels = c( "Underweight", "Healthy Weight", "Overweight", "Obesity"))
  }
  # check output group detail
  else if(depth == 'full' & style == 'num') {
    # main function
    output <- factor(dplyr::if_else(x <18.5, "<18.5",
                                    dplyr::if_else(x >=18.5 & x <=25, "18.5-24.9",
                                                   dplyr::if_else(x >25 & x <30, "25.0-29.9",
                                                                  dplyr::if_else(x >=30 & x <35, "30.0-34.9",
                                                                                 dplyr::if_else(x >=35 & x <40, "35.0-39.9",
                                                                                                dplyr::if_else(x >=40, ">40.0", NA_character_
                                                                                                )))))), ordered = TRUE, levels = c("<18.5", "18.5-24.9", "25.0-29.9", "30.0-34.9", "35.0-39.9", ">40.0"))
  }
  # check output group detail
  else if(depth == 'part' & style == 'num') {
    # main function
    output <- factor(dplyr::if_else(x <18.5, "<18.5",
                                    dplyr::if_else(x >=18.5 & x <=25, "18.5-24.9",
                                                   dplyr::if_else(x >25 & x <30, "25.0-29.9",
                                                                  dplyr::if_else(x >=30, ">30.0", "NA"
                                                                  )))), ordered = TRUE, levels = c("<18.5", "18.5-24.9", "25.0-29.9", ">30.0"))
  }

  return(output)
}

