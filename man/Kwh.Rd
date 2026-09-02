% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kwh}
\alias{Kwh}
\title{Kruskal-Wallis H -- eqs. (10.4.2)/(10.4.5)/(10.4.7), pp. 354-358}
\usage{
Kwh(samples, correct = TRUE) 
}
\description{
Kruskal-Wallis H -- eqs. (10.4.2)/(10.4.5)/(10.4.7), pp. 354-358
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
\item{correct}{Argument \code{correct}; see Usage.}
}
\value{
A list with \code{statistic}, \code{h_raw}, \code{correction}, \code{df}, \code{p_value}, \code{rank_sums}, \code{rank_means}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Kwh(V)
}
