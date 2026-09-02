% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_aol}
\alias{fit_aol}
\title{Single-stage AOL (or plain OWL, for comparison)}
\usage{
fit_aol(R, A, H, propensity = NULL, method = "aol", prognostic = NULL, 
    shift = NULL, ridge = 1e-06) 
}
\arguments{
\item{R}{Numeric vector of outcomes.}
\item{A}{Integer vector of treatments.}
\item{H}{Matrix of covariates.}
\item{propensity}{Randomisation probabilities.}
\item{method}{Either \code{"aol"} or \code{"owl"}.}
\item{prognostic}{Optional prognostic values for AOL.}
\item{shift}{Optional additive constant for OWL.}
\item{ridge}{Ridge penalty for the weighted classifier.}
}
\description{
Computes the weights, fits a weighted linear classifier, and reports its inverse-probability value on the observed data. The return is a named list whose names match the Python RichResult payload keys.
}
\value{
A list with \code{estimate}, \code{value}, \code{rule},   \code{coef}, \code{weights}, \code{labels}, \code{weight_cv},   \code{method}, \code{n}, \code{n_flipped} (AOL only, else   \code{NULL}), \code{shift} (OWL only, else \code{NULL}) and   \code{reference}.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 40
H <- matrix(rnorm(n * 2), n, 2)
A <- sample(c(-1, 1), n, replace = TRUE)
R <- 1 + A * H[, 1] + rnorm(n, 0, 0.3)
r <- fit_aol(R, A, H)
str(r, max.level = 1)
}
