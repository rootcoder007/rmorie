% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_lagvec}
\alias{.morie_rg_lagvec}
\title{R(n) as eq. (3.155) wants it: current sample first, zeros before the}
\usage{
.morie_rg_lagvec(r, i, m)
}
\arguments{
\item{r}{A vector; indexed elementwise.}
\item{i}{Numeric; combined arithmetically in the body.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
record starts
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
.morie_rg_lagvec(r = c(1, 2, 3, 4, 5, 6, 7, 8), i = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L)
}
