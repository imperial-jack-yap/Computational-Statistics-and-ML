#Question 1
quantile_bisection <- function(cdf, alpha, epsilon = 0.00001, iter = 50){
  a <- -1
  b <- 1
  g <- function(x) cdf(x) - alpha
  while (g(a) * g(b) > 0){
    a <- a - 1
    b <- b + 1
  }
  for (t in seq_along(seq(1,iter,by = 1))){
    c = (a + b) / 2
    
    if (g(c) == 0){
      return (c)
    }else if (g(a) * g(c) < 0){
      b = c
    }else{
      a = c
    }
    if (abs(a - b) < epsilon){
      return ((a + b) / 2)
    }
  }
  return ((a + b)/2)
} 

#Normal Distribution bisection algorithm
quantile_bisection(pnorm, 0.975)
qnorm(0.975)

#Cauchy Distribution bisection algorithm
quantile_bisection(pcauchy, 0.975)
qcauchy(0.975)


#Question 2
extinction_func_iter <- function(lambda, s,  p_k_lim = 0.0001, epsilon = 0.0001, max_iter = 100){
  p_k_func <- function(k) lambda^k * exp(-lambda) / factorial(k)
  s_val <- c(s)
  
  for (t in seq_along(seq(1,max_iter,by=1))){
    k <- 0
    s_new <- 0
    p_k <- 1
    
    while (p_k > p_k_lim){
      p_k <- p_k_func(k)
      s_new <- s_new + p_k * s^k
      k <- k + 1
    }
    
    s_val <- c(s_val, s_new)
    
    if (abs(s_new - s) < epsilon){
      return (list(final = s_new, trajectory = s_val))
    }
    s <- s_new
  }
  
  return(list(final = s, trajectory = s_val))
}

print(extinction_func_iter(1.1, 0.2)$final)
res <- extinction_func_iter(0.5, 0)

plot(res$trajectory, type="o", pch=16, col="blue",
     main="Convergence of Extinction Probability",
     xlab="Iteration", ylab="s_t")
abline(h=res$final, lty=2, col="red")

#Plotting for lambda
extinction_prob <- numeric()
lambda_vec <- seq(0.1, 3, by = 0.1)
for (lambda in lambda_vec){
  res <- extinction_func_iter(lambda, 0.2)
  extinction_prob <- c(extinction_prob, res$final)
}
plot(lambda_vec, extinction_prob, type='o',
     pch = 16, col= 'red',
     main = "Extinction Probability of Lambdas",
     xlab = "Lambda", ylab = "Extinction Probability, P")

#Question 3

extinction_newton <- function(lambda, s, p_k_lim = 0.0001, epsilon = 0.0001, max_iter = 100){
  p_k_func <- function(k) lambda^k * exp(-lambda) / factorial(k)
  s_val <- c(s)
  for (t in 1:max_iter){
    p_k <- 1
    k <- 0
    P_sum <- 0
    dP_sum <- 0
    s_new <- 0
    
    while (p_k > p_k_lim){
      p_k <- p_k_func(k)
      P_sum <- P_sum + p_k * s^k
      dP_sum <- dP_sum + k * p_k * s^(k-1)
      k <- k + 1
    }
    s_new <- s - (P_sum - s)/(dP_sum - 1)
    s_val <- c(s_val, s_new)
    
    if (abs(s - s_new) < epsilon){
      return (list(final = s_new, trajectory = s_val))
    }
    
    s <- s_new
  }
  
  return (s)
}


plot(extinction_func_iter(1.1,0.2)$trajectory, type="o", pch=16, col="blue",
     main="Convergence of Extinction Probability",
     xlab="Iteration", ylab="s_t")
lines(extinction_newton(1.1, 0.2)$trajectory , type="o", pch=16, col="red")
legend("bottomright", legend = c("Functional Iteration, Newton-Raphson"),
       col = c('blue','red'), lty = 1)
abline(h = extinction_newton(1.1, 0.2)$final, col='green')
