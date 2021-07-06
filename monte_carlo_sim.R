# This shows how to predict stock prices using Monte Carlo simulation
# Link: https://www.youtube.com/watch?v=Dl3qp5wiPsQ&list=WL&index=18
library(quantmod)
library(lubridate)
source("helper_functions/prepare_data.R")
source("helper_functions/stk_ret.R")


# Prepare data ------------------------------------------------------------

td          <- Sys.Date()
prep_data   <- prepare_data(ticker = "SPY", today = td)


# Define functions --------------------------------------------------------

# Prepare custom functions to run the simulation. Use random probabilities.
stk_ret <- function(stk_prc   = NULL, 
                    n_periods = 1, 
                    mn_ret    = NULL, 
                    std_ret   = NULL){
  delta_t <- 1 / n_periods
  for (i in seq(n_periods)){
    eps <- runif(n = 1, min = 0, max = 1)
    adj <- qnorm(eps, mn_ret * delta_t, std_ret * sqrt(delta_t))
    stk_prc <- stk_prc * (1 + adj)
  }
  return(stk_prc)
}

mean_rets <- function(compute_until = NULL){
  tmp_data               <- tmp
  temp_df                 <- tmp_data[paste0("::", compute_until)]
  temp_df                 <- ROC(temp_df, type = "discrete")
  temp_df[is.na(temp_df)] <- 0
  out                     <- mean(temp_df)
  return(out)
}

stdv_rets <- function(compute_until = NULL){
  tmp_data               <- tmp
  temp_df                 <- tmp_data[paste0("::", compute_until)]
  temp_df                 <- ROC(temp_df, type = "discrete")
  temp_df[is.na(temp_df)] <- 0
  out                     <- sd(temp_df)
  return(out)
}

# Run the simulations -----------------------------------------------------




 