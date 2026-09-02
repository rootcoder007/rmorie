% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DivAv}
\alias{DivAv}
\title{The book averages the pairwise divergences for a single measure over}
\usage{
DivAv(means, covs) 
}
\arguments{
\item{means}{A vector; its length is taken and its elements indexed.}
\item{covs}{A vector; its length is taken and its elements indexed.}
}
\description{
m classes.  Averaging hides a badly separated PAIR behind well separated ones, so the minimum is returned too -- that is the pair that will actually be confused.
}
\value{
A list with \code{average}, \code{pairwise}, \code{minimum}, \code{worst_pair}, \code{n_classes}, \code{n_pairs}, \code{average_hides_the_worst_pair}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DivAv(V, V)
}
