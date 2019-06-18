### eGFR to CKD groups

grp_ckd <- function(x, style = 'name') { ## x is numeric eGFR variable
  # check input type, error message
  if(!is.numeric(x)) {
    stop("eGFR input value must be numeric", call. = FALSE)
  }
  # check output detail
  else if(style == 'name') {
    # main function
    output <- factor(dplyr::if_else(x <15.0, "Stage 5",
                                    dplyr::if_else(x >=15.0 & x <30, "Stage 4",
                                                   dplyr::if_else(x >=30 & x <45, "Stage 3B",
                                                                  dplyr::if_else(x >=4 & x <60, "Stage 3A",
                                                                                 dplyr::if_else(x >=60 & x <90, "Stage 2",
                                                                                                dplyr::if_else(x >=90, "Stage 1", "NA"
                                                                                                )))))), ordered = TRUE)
  }
  # check output detail
  else if(style == 'num') {
    # main function
    output <- factor(dplyr::if_else(x <15.0, "<15",
                                    dplyr::if_else(x >=15.0 & x <30, "15-30",
                                                   dplyr::if_else(x >=30 & x <45, "30-45",
                                                                  dplyr::if_else(x >=4 & x <60, "45-60",
                                                                                 dplyr::if_else(x >=60 & x <90, "60-90",
                                                                                                dplyr::if_else(x >=90, "90+", "NA"
                                                                                                )))))), ordered = TRUE)
  }
  
  return(output)
}
