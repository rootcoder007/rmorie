% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{aol_weights}
\alias{aol_weights}
\title{AOL weights |R_i - m(H_i)|/pi_i}
\usage{
aol_weights(R, A, H, propensity = NULL, prognostic = NULL, ridge = 1e-08) 
}
\arguments{
\item{R}{Numeric vector of outcomes.}
\item{A}{Integer vector of treatments, coded -1 or +1.}
\item{H}{Matrix of covariates (one row per subject).}
\item{propensity}{Randomisation probabilities, scalar or vector.}
\item{prognostic}{Optional numeric vector of fitted prognostic   values; if \code{NULL} it is estimated by ridge-regularised   least squares.}
\item{ridge}{Ridge penalty for the prognostic fit.}
}
\description{
Augmented outcome-weighted-learning weights of Liu et al. (2018). The prognostic part \code{m(H)} is removed from the outcome before weighting, so a negative residual is not shifted away but flips the label: a subject who did worse than predicted under the arm they received is evidence for the other arm. If \code{prognostic} is omitted it is fitted by least squares on H alone -- deliberately without treatment, since the point is to remove the part of the outcome common to both arms.
}
\value{
A list with \code{weights}, \code{labels} (possibly with   some signs flipped), \code{residual}, \code{prognostic},   \code{n_flipped}, \code{cv} and \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 40
H <- matrix(rnorm(n * 2), n, 2)
A <- sample(c(-1, 1), n, replace = TRUE)
R <- 1 + A * H[, 1] + rnorm(n, 0, 0.3)
aol_weights(R, A, H)
}
