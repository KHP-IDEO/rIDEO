# hba1c targets


tgt_hba1c <- function(x, y) { # x is df to be added to [mutated] # y is numeric hba1c variable within x which is being compared to target levels
  if(!is.numeric(y)) {
    stop("HbA1c input value must be numeric", call. = FALSE)
  }
  else {
    hba1c_48 <- as.logical(if_else(x < 48, 1, 0)) # 6.5%
    hba1c_53 <- as.logical(if_else(x <= 53, 1, 0)) # 7.0%
    hba1c_58 <- as.logical(if_else(x <= 58, 1, 0)) # 7.5%
    hba1c_64 <- as.logical(if_else(x <= 64, 1, 0)) # 8.0%
    hba1c_69 <- as.logical(if_else(x <= 69, 1, 0)) # 8.5%
    hba1c_75 <- as.logical(if_else(x <= 75, 1, 0)) # 9.0%
    hba1c_86 <- as.logical(if_else(x <= 86, 1, 0)) # 10.0%
  }
  return(mutate(x, hba1c_48 = hba1c_48, hba1c_53 = hba1c_53, hba1c_58 = hba1c_58, hba1c_64 = hba1c_64, hba1c_69 = hba1c_69, hba1c_75 = hba1c_75, hba1c_86 = hba1c_86))
}