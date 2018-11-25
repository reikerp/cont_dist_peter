# Peter Reiker
# 11/21/2018
# Professor Speegle
# Project_2018
# Description:
# Sample from a 2D Distribution Given FX(x) and F(y|X = x)
# This function samples from a 2D distribution given the marginal pdf of X and the conditional pdf of Y given X = x.
# Its output is in the form of a vector where a sample from the marginal pdf of X is the first element.

# Input variables:
# n is the number of samples to generate.
# fx is the marginal pdf of X.
# fyx is the conditional pdf of Y given X = x (it is in the form function(y,x) expression).

# Return Value:
# my_samples is a list of samples from the desired 2D distribution.

# Example:
# d2_sampler_special(3, function(x) {exp(-x^2/2)/sqrt(2*pi)}, function(y,x){exp(-y^2/2)/sqrt(2*pi)})

# REMEMBER TO MAKE SURE THIS WORKS WHEN IN PACAKAGE FORM.
d2_sampler_special <- function(n = 1, fx, fyx) {
  # Determine the values to be used with the rejection_sampling function (first time).
  a = 0
  b = 0

  while(integrate(fx, -Inf, a)$value > 1/10^4) a = a - 1
  while(integrate(fx, b, Inf)$value > 1/10^4) b = b + 1

  # Now find an approximate value for C (first time).
  C = optimize(fx, c(a,b), maximum = T)

  one_sample = function(fx, fyx, a, b, C){
  # Now use rejection sampling to find a value of x.
  x = rejection_sampling(1, fx, a, b, C$maximum)

  # Repeat the same process to find a sample from the conditional pdf of Y given X = x.
  fyx_given_x = function(y) fyx(y,x)

  j = 0
  k = 0

  while(integrate(fyx_given_x, -Inf, j)$value > 1/10^4) j = j - 1
  while(integrate(fyx_given_x, k, Inf)$value > 1/10^4) k = k + 1

  D = optimize(fyx_given_x, c(j,k), maximum = T)

  y = rejection_sampling(1, fyx_given_x, j, k, D$maximum)

  # Return the desired sample as a vector.
  return (c(x,y))
  }

  my_samples = list()
  # Here I break up the sampling process using my previously defined function.
  for (i in 1:n) {
    my_samples[[i]] = one_sample(fx, fyx, a, b, C)
  }
  return(my_samples)
}
