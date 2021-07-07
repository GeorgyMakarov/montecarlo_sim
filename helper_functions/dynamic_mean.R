#' Compute dynamic means
#' 
#' Compute dynamic means based on expiry dates and adjusted stock prices.
#'
#' @param input_data a list of inputs
#' @param compute_until a date until to compute mean
#'
#' @return numeric vector
#' @export
#'
dynamic_mean <- function(input_data = NULL, compute_until = NULL){
  tmp             <- input_data$adjusted
  tmp             <- tmp[paste0("::", compute_until)]
  tmp             <- TTR::ROC(tmp, type = "discrete")
  tmp[is.na(tmp)] <- 0
  mean(tmp)
}