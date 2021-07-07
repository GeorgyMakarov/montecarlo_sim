#' Run stock price simulation
#' 
#' Run stock price simulation without volatility, based on mean return, standard
#' deviation and historical adjusted prices data.
#'
#' @param input_data a list of historical data: stock, adjusted, mean, sd
#' @param n_sims number of simulations to run
#' @param n_days number of days to simulate the prices for
#' @param today current date in format "yyyy-mm-dd"
#'
#' @return a numeric vector of length == n_sims
#' @export
run_sim <- function(input_data = NULL,
                    n_sims     = NULL,
                    n_days     = NULL,
                    today      = NULL){
  
  tmp     <- input_data$adjusted
  c_price <- as.numeric(zoo::coredata(tmp[today - lubridate::days(n_days)]))
  
  stock_prices <- c()
  for (i in seq(n_sims)){
    out          <- stk_ret(stk_prc   = c_price, 
                            n_periods = n_days, 
                            mn_ret    = input_data$avg_ret,
                            sd_ret    = input_data$sd_ret)
    stock_prices <- c(stock_prices, out)
  }
  
  return(stock_prices)
  
}