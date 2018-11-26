#' @title Single Variable Rejection Sampling
#'
#' @author Peter Reiker
#' @date 11/26/2018
#'
#' @description
#' This is the main function of the MATH 4800 project.
#' It recieves a function's pdf and generates a number of samples from the rv with the provided pdf using rejection sampling.
#' It is thereby somewhat similar to functions such as rbinom, runif, etc.
#'
#' @param  n is the number of samples to generate.
#' @param pdf is the pdf of the random variable you wish to sample from.
#' @param a is the lower bound of the random variable you wish to sample from.
#' @param b is the upper bound of the random variable you wish to sample from.
#' @param C is a numeric such that f(x) <= C for all values of x.
#'
#' @return my_sample A vector containg the desired number of random samples from the desired distribution.
#' @export
#'
#' @examples
#' rejection_sampling(10^4, function(x)1, 0, 1, 1)
#' rejection_sampling(10^4, function(x)2*x, 0, 1, 2)
#' rejection_sampling(10^4, function(x)exp(-x), 0, 10, 1)

rejection_sampling <- function(n = 1, pdf, a = 0, b = 1, C) {
  # Here I use recursion to obtain the value of a single sample.
  one_sample = function(pdf, a, b, C){
    test_draw = runif(1, a, b)
    if (runif(1, 0, C) <= pdf(test_draw)) return(test_draw)
    return(one_sample(pdf, a, b, C))
  }
  # Here I break the sampling process up using my previously defined function.
  for (i in 1:n){
    if (i == 1) my_samples = one_sample(pdf, a, b, C)
    if (i != 1) my_samples = c(my_samples, one_sample(pdf, a, b, C))
  }
  return (my_samples)
}
