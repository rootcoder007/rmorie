% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.strtwt_fit}
\alias{.strtwt_fit}
\title{.strtwt_fit}
\usage{
.strtwt_fit(Z, a)
}
\arguments{
\item{Z}{Passed to \code{.s03logit}.}
\item{a}{Passed to \code{.s03logit}.}
}
\description{
A step of the Strtwt implementation. Called by \code{Strtwt}. See the file header for the source the module follows.
}
\value{
The value of \code{pmin}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.strtwt_fit(M, V)
}
