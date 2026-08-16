% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_medweight_point}
\alias{.morie_medweight_point}
\title{The four normalised means, exactly as written in Huber (2014)}
\usage{
.morie_medweight_point(y, d, pm, px)
}
\arguments{
\item{y}{Passed to \code{.morie_wmean}.}
\item{d}{Numeric; combined arithmetically in the body.}
\item{pm}{Numeric; combined arithmetically in the body.}
\item{px}{Numeric; combined arithmetically in the body.}
}
\description{
Section 3. Reading the two subtracted terms of theta(1) and theta(0) as potential-outcome means gives, in order, E[Y(1,M(1))], E[Y(0,M(1))], E[Y(1,M(0))], E[Y(0,M(0))].
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
.morie_medweight_point(y = c(1, 2, 3, 4, 5, 6, 7, 8), d = 5L, pm = c(1, 2, 3, 4, 5, 6, 7, 8), px = c(1, 2, 3, 4, 5, 6, 7, 8))
}
