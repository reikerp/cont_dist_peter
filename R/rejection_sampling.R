# Peter Reiker
# 11/21/2018
# Professor Speegle
# Project_2018
# Description:
# This is the main function of this project.
# It recieves a function's pdf and generates a number of samples from the rv with the provided pdf using rejection sampling.
# It is thereby somewhat similar to functions such as rbinom, runif, etc.

# Input variables:
# n is the number of samples to generate.
# pdf is the pdf of the random variable you wish to sample from.
# a is the lower bound of the random variable you wish to sample from.
# b is the upper bound of the random variable you wish to sample from.
# C is a numeric such that f(x) <= C for all values of x.

rejection_sampling <- function(n = 1, pdf, a = 0, b = 1, C) {
  # Here I use recursion to obtain the value of a single sample.
  one_sample = function(pdf, a, b, C){
    test_draw = runif(1, a, b)
    if (pdf(test_draw) <= C) return(test_draw)
    return(one_sample(pdf, a, b, C))
  }
  # Here I break the sampling process up using my previously defined function.
  for (i in 1:n){
    if (i == 1) my_samples = one_sample(pdf, a, b, C)
    if (i != 1) my_samples = c(my_samples, one_sample(pdf, a, b, C))
  }
  return (my_samples)
}
