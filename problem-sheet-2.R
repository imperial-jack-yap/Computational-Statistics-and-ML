em_exp <- function(theta_start, t, N, M, epsilon = 0.0001, max_iter = 100){
  theta_old <- theta_start
  theta_new <- theta_start
  for (i in 1:max_iter){
    exp_failed <- M* (theta_old - (t * exp(-t/theta_old))/(1 - exp(-t/theta_old)))
    exp_censored <- (N - M) * (t + theta_old)
    theta_new <- 1 / N * (exp_failed - exp_censored)
    
    if (abs(theta_old - theta_new) < epsilon){
      return (theta_new)
    }
    
    theta_old <- theta_new
  
  }
  return (theta_new)
}

                                                                                                                                                                                                                                                                                                                  