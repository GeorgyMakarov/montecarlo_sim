#' Prepare financial data for Monte Carlo simulations
#' 
#' Prepares financial data from Yahoo finance using `quantmod`, `xts`, `TTR`
#' libraries. This is a helper function to save coding required to get the
#' data appropriate for Monte Carlo simulation.
#'
#' @param ticker a letter code of a stock in question
#' @param today a character string of current date in `yyyy-mm-dd` format
#' @param out a desired output: adjusted, stock table, mean, standard deviation
#'
#' @return a list of numeric values: xts objects, num vectors
#' @export
#'
#' @examples
#' prepare_data(ticker = "SPY", today = "2021-07-05", out = "mean")
prepare_data <- function(ticker = NULL, 
                         today  = NULL){
  
  ticker  <- toupper(ticker)
  stock   <- quantmod::getSymbols(ticker, auto.assign = F)
  current <- quantmod::getQuote(ticker)
  stock   <- rbind(stock, 
                   xts::xts(
                     cbind(current$Open, 
                           current$High, 
                           current$Low, 
                           current$Last, 
                           current$Volume, 
                           current$Last), 
                     order.by = as.Date(today)))
  
  adjusted          <- quantmod::Ad(stock)
  rets              <- TTR::ROC(adjusted, type = "discrete")
  rets[is.na(rets)] <- 0
  avg_ret           <- mean(rets)
  sd_ret            <- sd(rets)
  
  res <- list(stock, adjusted, avg_ret, sd_ret)
  names(res) <- c("stock", "adjusted", "avg_ret", "sd_ret")
  return(res)
  
}