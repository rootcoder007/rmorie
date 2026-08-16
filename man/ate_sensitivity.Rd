% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ate_sensitivity}
\alias{ate_sensitivity}
\title{L1 sensitivity of a TMLE of the ATE}
\usage{
ate_sensitivity(n, g_min, y_range = 1)
}
\arguments{
\item{n}{Sample size.}
\item{g_min}{Propensity truncation bound.}
\item{y_range}{Outcome range.}
}
\description{
One observation enters through the clever covariate, so the bound carries 1/g_min: it is 2 R / (n g_min), not O(1/n). Truncating the propensity score is what makes the release affordable.
}
\value{
A list with \code{sensitivity}, \code{naive_1_over_n},   \code{inflation}, \code{g_min}, \code{n}, \code{note}.
}
\keyword{internal}
\examples{
ate_sensitivity(n = 100, g_min = 0.05)
}
