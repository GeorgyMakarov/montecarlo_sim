#' Compute dynamic mean and standard deviation
#'
#' @param input_data a list of input values
#' @param next_expiry date until which to compute values
#'
#' @return a list of three numeric vectors
#' @export
#'
compute_dynamics <- function(input_data  = NULL,
                             next_expiry = NULL){
  
  tmp      <- input_data$adjusted
  expiry   <- tmp[quantmod::options.expiry(tmp)]
  expiry   <- expiry["2007::"]
  indices  <- zoo::index(expiry)
  n_expiry <- as.Date(next_expiry)
  indices  <- c(indices, n_expiry)
  
  mn_rets <- do.call(rbind,
                     lapply(X   = as.list(indices), 
                            FUN = function(x){
                              dynamic_mean(input_data    = input_data,
                                           compute_until = x)
                            }))
  
  sd_rets <- do.call(rbind,
                     lapply(X   = as.list(indices),
                            FUN = function(x){
                              dynamic_sd(input_data    = input_data,
                                         compute_until = x)
                            }))
  
  idx_diffs <- as.numeric(diff(indices))
  
  res <- list("avg_rets" = mn_rets, "sd_rets" = sd_rets, "days" = idx_diffs)
  return(res)
}