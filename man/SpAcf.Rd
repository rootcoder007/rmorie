% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpAcf}
\alias{SpAcf}
\title{Empirical correlogram R(h) = C(h)/C(0), Schabenberger & Gotway (2005)}
\usage{
SpAcf(coords, z, bins = NULL, cutoff = NULL)
}
\arguments{
\item{coords}{See Usage.}
\item{z}{See Usage.}
\item{bins}{Defaults to \code{NULL}.}
\item{cutoff}{Defaults to \code{NULL}.}
}
\description{
Sec. 1.4.2 and Chapter problem 1.14.  C(0) uses the 1/n divisor.
}
\value{
A list with \code{lags}, \code{centres}, \code{cov}, \code{acf}, \code{c0}, \code{npairs}, \code{n}, \code{incomplete_description_of_second_order_structure}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SpAcf(V, V)
}
