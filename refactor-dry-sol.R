library(checkmate)

x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
v <- z^2

# computes a 95% confidence interval based on the normal distribution assumption
# input: numeric vector x
# output: lower and upper bounds of the 95% confidence interval
confidence_interval <- function(x){
  assert_numeric(x, finite = TRUE, min.len = 1)
  
  empir_mean <- mean(x)
  empir_sd <- sd(x)
  xlen <- length(x)
  half_length <- 1.96 * empir_sd / sqrt(xlen)
  
  return(c(empir_mean - half_length, empir_mean + half_length))
}

confidence_interval(x)
confidence_interval(y)
confidence_interval(z)
confidence_interval(v)
