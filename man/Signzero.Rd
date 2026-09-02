% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signzero}
\alias{Signzero}
\title{Zero differences in the sign test -- Sec. 5.4.8, p. 180}
\usage{
Signzero(x, m0 = 0, method = "discard") 
}
\description{
Zero differences in the sign test -- Sec. 5.4.8, p. 180
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{m0}{Argument \code{m0}; see Usage.}
\item{method}{Argument \code{method}; see Usage.}
}
\value{
A list with \code{statistic}, \code{n}, \code{nzero}, \code{k_raw}, \code{n_raw}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Signzero(V)
}
