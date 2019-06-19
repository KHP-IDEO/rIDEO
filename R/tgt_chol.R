# cholesterol targets


tgt_chol <- function(x, y) { # x is df to be added to [mutated] # y is numeric cholesterol variable within x which is being compared to target levels
  if(!is.numeric(y)) {
    stop("Cholesterol input value must be numeric", call. = FALSE)
  }
  else {
    chol_40 <- as.logical(dplyr::if_else(x <= 4.0, 1, 0))
    chol_50 <- as.logical(dplyr::if_else(x <= 5.0, 1, 0))
    chol_75 <- as.logical(dplyr::if_else(x <= 7.5, 1, 0))
    chol_90 <- as.logical(dplyr::if_else(x <= 9.0, 1, 0))
  }
  return(dplyr::mutate(x, chol_4 = chol_40, chol_5 = chol_50, chol_75 = chol_75, chol_90 = chol_90))
}
