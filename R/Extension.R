
#' pnormmix -> CDF of two noraml distributions
#'
#' @param x The values where the density is evaluated
#' @param mean1 The mean of the first normal distribution
#' @param sd1 The standard deviation of the first normal distribution
#' @param mean2 The mean of the second normal distribution
#' @param sd2 The standard deviation of the second normal distribution
#' @param mixprob The proportion of the first distribution included. 
#'
#' @returns
#' @export
#'
#' @examples
pnormmix <- function(x, mean1, sd1, mean2, sd2, mixprob) {
  
  mixprob * pnorm(x, mean = mean1, sd = sd1) + (1 - mixprob) * pnorm(x, mean = mean2, sd = sd2)
}

#' dnormmix -> pdf of two normal distributions
#'
#' @param x The values where the density is evaluated
#' @param mean1 The mean of the first normal distribution
#' @param sd1 The standard deviation of the first normal distribution
#' @param mean2 The mean of the second normal distribution
#' @param sd2 The standard deviation of the second normal distribution
#' @param mixprob The proportion of the first distribution included. 
#'
#' @returns
#' @export
#'
#' @examples
dnormmix <- function(x, mean1, sd1, mean2, sd2, mixprob) {
  
  mixprob * dnorm(x, mean = mean1, sd = sd1) + (1 - mixprob) * dnorm(x, mean = mean2, sd = sd2)
}
#Using Vectorize() so qnormmix can run multiple points.

#' qnormmix -> inverse CDF of two normal distributions
#'
#' @param p The probabilities at which the inverse CDF is evaluated
#' @param mean1 The mean of the first normal distribution
#' @param sd1 The standard deviation of the first normal distribution
#' @param mean2 The mean of the second normal distribution
#' @param sd2 The standard deviation of the second normal distribution
#' @param mixprob The proportion of the first distribution included. 
#'
#'
#' @returns
#' @export
#'
#' @examples
qnormmix <- Vectorize(function(p, mean1, sd1, mean2, sd2, mixprob) {
  
  quan2 <- function(prob) {
    function(x) pnormmix(x, mean1, sd1, mean2, sd2, mixprob) - prob
  }
  uniroot(quan2(p), interval = c(-10, 10), extendInt = "yes")$root
})


#' rnormmix -> Generates random numbers from a mixture of two normal distributions
#'
#' @param n The number of random samples to generate
#' @param mean1 The mean of the first normal distribution
#' @param sd1 The standard deviation of the first normal distribution
#' @param mean2 The mean of the second normal distribution
#' @param sd2 The standard deviation of the second normal distribution
#' @param mixprob The proportion of the first distribution included. 
#'
#' @returns
#' @export
#'
#' @examples
rnormmix <- function(n, mean1, sd1, mean2, sd2, mixprob) {
  
  flips2 <- rbinom(n, 1, mixprob)  # 1 with prob mixprob, 0 otherwise
  rvs2 <- ifelse(flips == 1, rnorm(n, mean = mean1, sd = sd1), rnorm(n, mean = mean2, sd = sd2))
  return(rvs2)
  
}