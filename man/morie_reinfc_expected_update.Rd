% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_reinfc_expected_update}
\alias{morie_reinfc_expected_update}
\title{morie_reinfc_expected_update}
\usage{
morie_reinfc_expected_update(p, r0, r1, alpha = 1, b = 0) 
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{r0}{Numeric; combined arithmetically in the body.}
\item{r1}{Numeric; combined arithmetically in the body.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{b}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the reinfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
# For a Bernoulli-logistic unit the expected update is
# alpha * p * (1 - p) * (r1 - r0), which is alpha times the gradient of
# expected reward, and does not depend on the baseline at all.
morie_reinfc_expected_update(p = 0.5, r0 = 0, r1 = 1, alpha = 2)
# the same update for any baseline (equation 10)
morie_reinfc_expected_update(p = 0.5, r0 = 0, r1 = 1, alpha = 2, b = 100)
}
