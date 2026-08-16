% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BayesNorm}
\alias{BayesNorm}
\title{Eq (10.72).  The book takes logarithms at eq (10.71) because the}
\usage{
BayesNorm(x, means, covs, priors = NULL, full = FALSE)
}
\arguments{
\item{x}{See Usage.}
\item{means}{See Usage.}
\item{covs}{See Usage.}
\item{priors}{Defaults to \code{NULL}.}
\item{full}{Defaults to \code{FALSE}.}
}
\description{
normal PDF is an exponential and ln is monotonic: the ranking is unchanged while the arithmetic stops underflowing.  It then drops the (n/2) ln(2 pi) term, which "does not depend upon i", giving eq (10.73) -- safe for CLASSIFYING, wrong for reading the value as a log density, so both forms are returned.  The surfaces are hyperquadrics and reduce to hyperplanes exactly when the covariances are equal.
}
\value{
A list with \code{d}, \code{d_full}, \code{d_dropped_constant}, \code{assigned}, \code{priors}, \code{constant_term}, \code{surfaces_are_hyperquadrics}, \code{linear_when_covariances_are_equal}, \code{log_form_avoids_underflow}, \code{method}.
}
\keyword{internal}
