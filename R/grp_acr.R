### ACR to albuminuria groups

grp_acr <- function(x, style = 'name') { ## x is numeric ACR variable
  # check input type, error message
  if(!is.numeric(x)) {
    stop("ACR input value must be numeric", call. = FALSE)
  }
  # check output detail
  else if(style == 'name') {
    # main function
    output <- factor(dplyr::if_else(x <3.0, "A1",
                                    dplyr::if_else(x >=3.0 & x <=30, "A2",
                                                   dplyr::if_else(x >30, "A3", "NA"))),
                     ordered = TRUE, levels = c("A1", "A2", "A3"))
  }
  # check output detail
  else if(style == 'num') {
    # main function
    output <- factor(dplyr::if_else(x <3.0, "<3",
                                    dplyr::if_else(x >=3.0 & x <=30, "3-30",
                                                   dplyr::if_else(x >30, ">30", "NA"))),
                     ordered = TRUE, levels = c("<3", "3-30", ">30"))
  }

  # check output detail
  else if(style == 'alb') {
    # main function
    output <- factor(dplyr::if_else(x <3.0, "Normal albuminuria",
                                    dplyr::if_else(x >=3.0 & x <=30, "Microalbuminuria",
                                                   dplyr::if_else(x >30, "Macroalbuminuria", "NA"))), ordered = TRUE)
  }

  return(output)
}
