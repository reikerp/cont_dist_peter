\name{d2_sampler_special}
\alias{d2 sampler special}
\title{2D Sampler Special:}
\usage{
d2_sampler_special(n, fx, fyx)
}
\description{
This function samples from a 2D distribution given the marginal pdf of X and the conditional pdf of Y given X = x.
}
\examples{
d2_sampler_special(10, function(x) {exp(-x^2/2)/sqrt(2*pi)}, function(y,x){exp(-y^2/2)/sqrt(2*pi)})
d2_sampler_special(10, function(x) {ifelse(x > 0, exp(-x), 0)}, function(y,x){ifelse(0 < y & y < x, 1/x, 0)})
d2_sampler_special(10, function(x) {ifelse(0 < x & x < 1, 1, 0)}, function(y,x){ifelse(0 < y, x*exp(-y*x), 0)})
}
