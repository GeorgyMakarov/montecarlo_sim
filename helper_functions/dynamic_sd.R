#' Compute dynamic standard deviation
#' 
#' Compute dynamic standard deviation based on adjusted stock prices and
#' input date.
#'
#' @param input_data a list of input values
#' @param compute_until a date until which to compute
#'
#' @return a numeric vector
#' @export
#'
dynamic_sd <- function(input_data = NULL, compute_until = NULL){
  tmp             <- input_data$adjusted
  tmp             <- tmp[paste0("::", compute_until)]
  tmp             <- TTR::ROC(tmp, type = "discrete")
  tmp[is.na(tmp)] <- 0
  sd(tmp)
}
