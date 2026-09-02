% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Jtsum}
\alias{Jtsum}
\title{JT as the pairwise U matrix -- Sec. 10.6, eq. (10.6.1), p. 365}
\usage{
Jtsum(samples) 
}
\description{
JT as the pairwise U matrix -- Sec. 10.6, eq. (10.6.1), p. 365
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
}
\value{
A list with \code{u}, \code{statistic}, \code{npairs}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Jtsum(V)
}
