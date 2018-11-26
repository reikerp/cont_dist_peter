\name{rejection_sampling}
\alias{rejection sampling}
\title{Rejection Sampling:}
\usage{
rejection_sampling(n, pdf, a, b, C)
}
\description{
This function generates a number of samples from the rv with the provided pdf using rejection sampling.
}
\examples{
rejection_sampling(10^4, function(x)1, 0, 1, 1)
rejection_sampling(10^4, function(x)2*x, 0, 1, 2)
rejection_sampling(10^4, function(x)exp(-x), 0, 10, 1)
}
