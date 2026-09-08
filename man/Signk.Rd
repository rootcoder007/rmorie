% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signk}
\alias{Signk}
\title{Sign-test statistic K -- Sec. 5.4, eq. (5.4.1), p. 168}
\usage{
Signk(x, m0 = 0) 
}
\description{
Sign-test statistic K -- Sec. 5.4, eq. (5.4.1), p. 168
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{m0}{Argument \code{m0}; see Usage.}
}
\value{
A list with \code{statistic}, \code{n}, \code{nzero}, \code{mean}, \code{var}, \code{n_raw}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Signk(V)
}
