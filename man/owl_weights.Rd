% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{owl_weights}
\alias{owl_weights}
\title{OWL weights R_i/pi_i, with the additive constant}
\usage{
owl_weights(R, A, H, propensity = NULL, shift = NULL)
}
\arguments{
\item{R}{Numeric vector of outcomes.}
\item{A}{Integer vector of treatments, coded -1 or +1.}
\item{H}{Matrix of covariates (one row per subject).}
\item{propensity}{Randomisation probabilities, scalar or vector;   defaults to 0.5 for every subject.}
\item{shift}{Additive constant; if \code{NULL} the smallest value   that makes all weights non-negative is used.}
}
\description{
Computes the outcome-weighted-learning weights of Zhao et al. (2012). Because the weighted classifier needs non-negative weights and outcomes can be negative, OWL adds a constant \code{shift} to every outcome first. The default is the smallest value that makes all weights non-negative. The constant changes the relative weights, so the value used is reported back.
}
\value{
A list with \code{weights}, \code{labels} (the treatment   codes, unchanged), \code{shift} (the constant actually used),   \code{cv} (coefficient of variation of the weights) and   \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 40
H <- matrix(rnorm(n * 2), n, 2)
A <- sample(c(-1, 1), n, replace = TRUE)
R <- 1 + A * H[, 1] + rnorm(n, 0, 0.3)
owl_weights(R, A, H)
}
