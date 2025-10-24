set.seed(42)

unif_sampler <- function(N){
  u <- runif(1000, 0, 1)
  samples <- tan(pi * (u - 0.5))
  return(list(u = u, samples = samples))
}

empirical_cdf <- unif_sampler(1000)
x = seq(-100,100, length.out = 1000)
true_cdf <- pcauchy(x)

plot(ecdf(empirical_cdf$samples),
     xlab = "x",
     ylab = "F_X(x)")
lines(x, true_cdf, col='red', lwd = 2)
legend("topleft", legend = c("Empirical", "Theoretical"),
       col = c("Black", "Red"), lwd = 2)

#Exercise 3.3
rejection_sampling <- function(N){
  samples <- c()
  for (i in 1:N){
    u <- runif(1, 0, 1)
    x <-  rexp(1, 1/3)
    func <- 0.5 * (cos(x + 1) + 1)
    #
    if (u < func & x >= 0.5){
      samples <- c(samples, x)
    }
  }
  return(samples)
}
length(rejection_sampling(10000))

#Question 4
#Acceptance Probability for varied a
acceptance_prob <- function(a){
  if (a > 1){
    return (factorial(a - 1) * exp(a + 1) / (a + 1)^ (a + 1))
  }else{
    stop("a must be greater than 0")
  }
}

rs_gamma_cauchy <- function(N, a){
  samples <- c()
  if (a < 1){
    stop("a must be greater than 0")
  }
  c <-  (a + 1)^ (a + 1) / gamma(a) * exp(-(a + 1)) * 2 * pi
  for (i in 1:N){
    u <- runif(1, 0, 1)
    x <- rcauchy(1)
    
    func <- (pi/factorial(a - 1) * x^(a - 1) * exp(-x) * (1 + x^2))/c
    if (u < func & x > 0){
      samples <- c(samples, x)
    }
  }
  return (samples)
}
length(rs_gamma_cauchy(1000,2))
acceptance_prob(2)

