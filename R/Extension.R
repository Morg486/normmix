
pnormmix <- function(x, mean1, sd1, mean2, sd2, mixprob) {
  mixprob * pnorm(x, mean = mean1, sd = sd1) + (1 - mixprob) * pnorm(x, mean = mean2, sd = sd2)
}

dnormmix <- function(x, mean1, sd1, mean2, sd2, mixprob) {
  mixprob * dnorm(x, mean = mean1, sd = sd1) + (1 - mixprob) * dnorm(x, mean = mean2, sd = sd2)
}
#Using Vectorize() so qnormmix can run multiple points.
qnormmix <- Vectorize(function(p, mean1, sd1, mean2, sd2, mixprob) {
  quan2 <- function(prob) {
    function(x) pnormmix(x, mean1, sd1, mean2, sd2, mixprob) - prob
  }
  uniroot(quan2(p), interval = c(-10, 10), extendInt = "yes")$root
})

rnormmix <- function(n, mean1, sd1, mean2, sd2, mixprob) {
  flips2 <- rbinom(n, 1, mixprob)  # 1 with prob mixprob, 0 otherwise
  rvs2 <- ifelse(flips == 1, rnorm(n, mean = mean1, sd = sd1), rnorm(n, mean = mean2, sd = sd2))
  return(rvs2)
}