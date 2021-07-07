#' Compute stock price
#' 
#' Compute stock price for a certain number of days from mean returns, standard
#' deviation and current stock price.
#'
#' @param stk_prc stock price to start from
#' @param n_periods number of days to compute the price for
#' @param mn_ret historical mean return
#' @param sd_ret historical standard deviation
#'
#' @return a numeric value
#' @export
#'
#' @examples
#' stk_ret(stk_prc = 400, n_periods = 20, mn_ret = 0.0005, sd_ret = 0.012)
stk_ret <- function(stk_prc   = NULL,
                    n_periods = 1,
                    mn_ret    = NULL,
                    sd_ret    = NULL){
  delta_t <- 1 / n_periods
  for (i in seq(n_periods)){
    eps <- runif(n = 1, min = 0, max = 1)
    k   <- qnorm(eps, mn_ret * delta_t, sd_ret * sqrt(delta_t))
    stk_prc <- stk_prc * (1 + k)
  }
  return(stk_prc)
}


