# This shows how to predict stock prices using Monte Carlo simulation
# Link: https://www.youtube.com/watch?v=Dl3qp5wiPsQ&list=WL&index=18
library(quantmod)
library(lubridate)
source("helper_functions/prepare_data.R")
source("helper_functions/stk_ret.R")
source("helper_functions/run_sim.R")
source("helper_functions/dynamic_mean.R")
source("helper_functions/dynamic_sd.R")
source("helper_functions/compute_dynamics.R")
source("helper_functions/run_montecarlo.R")


# Prepare the data for analysis. Use custom helper function to get the
# list of inputs for further models. This is to keep the code clear for
# understanding.
td          <- Sys.Date()
prep_data   <- prepare_data(ticker = "SPY", today = td)


# Compute stock prices without taking volatility into account. The output of the
# simulation shows that maximum predicted price is 1% above the historic data.
# This happens due to volatility and dynamic mean and standard deviation.
stk_res_novol <- run_sim(input_data = prep_data, 
                         n_sims     = 1e3, 
                         n_days     = 20, 
                         today      = td)
quantile(stk_res_novol)[5] / last(prep_data$adjusted)


# Compute stock prices with volatility and dynamic taken into account. Use
# expiry periods to compute the stock prices. Define number of iterations from
# the length of expiration dates vector -- do it manually here, as we want to
# keep this parameter flexible
dynams <- compute_dynamics(input_data  = prep_data, 
                           next_expiry = as.Date("2021-07-16"))
iter_l <- 1:171
iter_vect <- data.frame()
for (i in seq(iter_l)){
  test <- run_montecarlo(n_sims     = 1e2, 
                         iter       = i, 
                         l_run      = length(iter_l),
                         input_data = prep_data,
                         dynams     = dynams)
  iter_vect <- rbind(iter_vect, test)
  rm(i, test)
}
