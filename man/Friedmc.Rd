% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Friedmc}
\alias{Friedmc}
\title{Friedman multiple comparisons -- eq. (12.2.13), p. 445}
\usage{
Friedmc(rank_sums, k, alpha = 0.2) 
}
\description{
Friedman multiple comparisons -- eq. (12.2.13), p. 445
}
\arguments{
\item{rank_sums}{Argument \code{rank_sums}; see Usage.}
\item{k}{Argument \code{k}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{bound}, \code{zstar}, \code{diffs}, \code{significant}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
Friedmc(rank_sums = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
