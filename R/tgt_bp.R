# Blood Pressure targets

tgt_bp <- function(x, y, z) { # x is df to be added to [mutated] # y is numeric systolic bp variable & z is numeric diastolic bp variable, within x which are being compared to target levels
  if(!is.numeric(y) | !is.numeric(z)) {
    stop("BP input values must be numeric", call. = FALSE)
  }
  else {
    bp_13080 <- as.logical(if_else(y < 130 & z < 80, 1, 0))
    bp_14080 <- as.logical(if_else(y < 140 & z < 80, 1, 0))
    bp_14090 <- as.logical(if_else(y < 140 & z < 90, 1, 0))
    bp_15090 <- as.logical(if_else(y < 150 & z < 90, 1, 0))
  }
  return(mutate(x, bp_13080 = bp_13080, bp_14080 = bp_14080, bp_14090 = bp_14090, bp_15090 = bp_15090))
}