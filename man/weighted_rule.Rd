% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{weighted_rule}
\alias{weighted_rule}
\title{Weighted linear classifier, returned as a decision function}
\usage{
weighted_rule(H, labels, weights, ridge = 1e-06) 
}
\arguments{
\item{H}{Matrix of covariates (one row per subject).}
\item{labels}{Numeric vector of -1/+1 labels.}
\item{weights}{Numeric vector of non-negative weights.}
\item{ridge}{Ridge penalty for the weighted fit.}
}
\description{
Minimises weighted squared error against the labels, the surrogate used here for the weighted 0-1 loss. The sign of the fit is the rule. A subject is labelled +1 if the decision function is non-negative and -1 otherwise.
}
\value{
A list with \code{rule} (a function from covariate vectors   to -1/+1) and \code{coef} (intercept followed by feature   coefficients).
}
\keyword{internal}
\examples{
weighted_rule(H = 0.5, labels = 5L, weights = 5L)
}
