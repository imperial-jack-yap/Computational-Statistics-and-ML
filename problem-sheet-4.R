#Question 4.1
n <- 1000000
f <- function(x) x^3
x <- rexp(n, 0.3)
f_x = f(x)
ci_95 <- c(mean(f_x) + sd(f_x)/sqrt(n)*qnorm(0.025),  mean(f_x) + sd(f_x)/sqrt(n)*qnorm(0.975))
mean(f(x))

#Question 4.2
n <- 100000
sigma <- 0.1
alpha <- 2
beta <- 2
x <- c(0.13, 0.51, 0.79, 0.6, 0.99, 1.05, 0.93, 0.85, 0.72, 0.51)

#Likelihood function
f <- function(x, theta, sigma){
  likelihood_prod <- 1
  for (i in 1:length(x)){
    exp_val <- -(sin(i * theta) - x[i])^2 / (2 * sigma^2)
    likelihood_prod = likelihood_prod * (1/(sqrt(2 * pi) * sigma) * exp(exp_val))
  }
  likelihood_prod
}

#Calculating likelihood values
theta <- rbeta(n, alpha, beta)
likelihood_values <- sapply(theta, function(t) f(x, t, sigma))
mean(likelihood_values)
ci_97 <- c(mean(likelihood_values) + sd(likelihood_values)/sqrt(n) * qnorm(0.015),
           mean(likelihood_values) + sd(likelihood_values)/sqrt(n) * qnorm(0.985))

#Question 4.3
print(1 - pt(0.1, df = 5))

n <- 10e4
x <- rt(n, df = 5)
mean(x > 0.1) 

#t-dsitrbution with 4 dof
y <- rt(n, df = 4)
mean((y > 0.1) * dt(y, df=5) / dt(y, df=4))

#exponential
lambda = 0.5
z <- 0.1 + rexp(n, rate=lambda)
mean(dt(z, df=5) / dexp(z - 0.1, rate=lambda))

#Question 4.4
f <- function(x) {
   if (x > 0.5){
    0.5563 * (cos(x + 1) + 1) * exp(-x/3)
   }else{
     0
   }
}

rejection_sampling <- function(){
  i <- 0
  while(1){
    u <- runif(1)
    x <- 0.5 + rexp(1, rate=1/3)
    i <- i + 1
    func <- 0.5 * (cos(x + 1) + 1)
    if (u < func){
      return(x)
    }
  }
}

N <- 100000
samples <- replicate(N, rejection_sampling())
mean(samples)

importance_sampling <- function(){
  y <- 0.5 + rexp(1, rate = 1/3)
  f <- 0.5563 * (cos(y + 1) + 1) *exp(-y/3)
  g <- (1/3) * exp(-(y-0.5)/3)
  return(f/g)
}

samples <- replicate(N, importance_sampling())
mean(samples)

#Exercise 4.5
#k = 1
k1 = 1
k2 = 4
k3 = 100
samples_1 <- replicate(k1, abs(rnorm(1)))
samples_2 <- replicate(k2, abs(rnorm(1)))
samples_3 <- replicate(k3, abs(rnorm(1)))

importance_sampling <- function(samples){
  y <- rt(1, df=3)
  phi <- mean(samples) - min(samples)
  f <- dnorm(y)
  g <- dt(y, df=3)
  
  return(phi*f/g)
}
N <- 10e4
is_sample1 <- replicate(N, importance_sampling(samples_1))
is_sample2 <- replicate(N, importance_sampling(samples_2))
is_sample3 <- replicate(N, importance_sampling(samples_3))


mean_is_samples <- c(mean(is_sample1), mean(is_sample2), mean(is_sample3))
ci_95_is_1 <- c(mean(is_sample1) + sd(is_sample1)/sqrt(N)*qnorm(0.025),
                mean(is_sample1) + sd(is_sample1)/sqrt(N)*qnorm(0.975))

ci_95_is_2 <- c(mean(is_sample2) + sd(is_sample2)/sqrt(N)*qnorm(0.025),
                mean(is_sample2) + sd(is_sample2)/sqrt(N)*qnorm(0.975))

ci_95_is_3 <- c(mean(is_sample3) + sd(is_sample3)/sqrt(N)*qnorm(0.025),
                mean(is_sample3) + sd(is_sample3)/sqrt(N)*qnorm(0.975))
