run_montecarlo <- function(n_sims     = NULL,
                           iter       = NULL,
                           l_run      = NULL,
                           input_data = NULL,
                           dynams     = NULL){
  
  tmp      <- input_data$adjusted
  expiry   <- tmp[quantmod::options.expiry(tmp)]
  expiry   <- expiry["2007::"]
  n_days   <- dynams$days[iter]
  
  base_price <- as.numeric(expiry[iter])
  base_mean  <- dynams$avg_rets[iter]
  base_stdev <- dynams$sd_rets[iter]
  price_vect <- c()
  
  # Run the simulations for a given number of times
  for (i in seq(n_sims)){
    out <- stk_ret(stk_prc   = base_price,
                   n_periods = n_days,
                   mn_ret    = base_mean,
                   sd_ret    = base_stdev)
    price_vect <- c(price_vect, out)
  }
  
  # Define starting parameters of the output data frame
  start_price <- as.data.frame(round(base_price, 2))
  start_date  <- zoo::index(expiry[iter])
  probs       <- as.data.frame(t(round(quantile(price_vect, 
                                                probs = seq(0, 1, 0.05)))))
  
  # Define two different endings if it is last iteration
  if (iter == l_run){
    end_price <- as.data.frame(NA)
    end_date  <- as.data.frame(NA)
  } else {
    end_price <- as.data.frame(as.numeric(round(expiry[iter + 1], 2)))
    end_date  <- zoo::index(expiry[iter + 1])
  }
  
  res <- cbind(start_price,
               start_date,
               probs,
               end_price,
               end_date)
  colnames(res)[1] <- "start_price"
  colnames(res)[24] <- "end_price"
  colnames(res)[25] <- "end_date"
  
  return(res)
}