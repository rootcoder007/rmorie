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
morie_reinfc_expected_update(p = 0.5, r0 = c(1, 2, 3, 4, 5, 6, 7, 8), r1 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
