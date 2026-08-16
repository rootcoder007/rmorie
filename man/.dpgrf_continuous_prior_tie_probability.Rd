% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpgrf_continuous_prior_tie_probability}
\alias{.dpgrf_continuous_prior_tie_probability}
\title{Continuous-prior tie probability}
\usage{
.dpgrf_continuous_prior_tie_probability()
}
\description{
The whole problem in one number: under any continuous prior the posterior probability that two regions are exactly equal is 0, so a boundary can only be defined by thresholding a difference.
}
\value{
A list with \code{probability} and \code{note}.
}
\keyword{internal}
\examples{
.dpgrf_continuous_prior_tie_probability()
}
