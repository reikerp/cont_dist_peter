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
# fx is the marginal pdf of X.
# fyx is the conditional pdf of Y given X = x.

# REMEMBER TO MAKE SURE THIS WORKS WHEN IN PACAKAGE FORM.
2d_sampler_special <- function(n = 1, fx, fyx) {
  # Determine the values to be used with the rejection_sampling function.
  a = 0
  b = 0

  while(integrate(fx, -Inf, a) > 1/10^4) a = a - 1
  while(integrate(fx, b, Inf) > 1/10^4) b = b + 1

}
