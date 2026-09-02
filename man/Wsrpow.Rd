% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrpow}
\alias{Wsrpow}
\title{Signed-rank power -- eqs. (5.7.13)-(5.7.14), p. 205}
\usage{
Wsrpow(n, p1, p2, alpha = 0.05) 
}
\description{
Signed-rank power -- eqs. (5.7.13)-(5.7.14), p. 205
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{p1}{Argument \code{p1}; see Usage.}
\item{p2}{Argument \code{p2}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{power}, \code{z_beta}, \code{shift}, \code{sd0}, \code{n}, \code{p1}, \code{p2}.
}
\keyword{internal}
\examples{
Wsrpow(n = 5L, p1 = c(1, 2, 3, 4, 5, 6, 7, 8), p2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
