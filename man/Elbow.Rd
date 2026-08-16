% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Elbow}
\alias{Elbow}
\title{WCSS falls monotonically with k and reaches zero at k = n, so it}
\usage{
Elbow(X, kmax = 8, kmin = 1)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{kmax}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{kmin}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
}
\description{
cannot be minimized -- the choice is the KNEE.  Located here as the point of maximum distance from the chord joining the curve\'s ends, which is a definite rule rather than an eye judgement.  Still a heuristic: on data with no cluster structure the curve is smooth and the knee is wherever the arithmetic puts it.
}
\value{
A list with \code{k}, \code{wcss}, \code{knee}, \code{monotonic}, \code{wcss_cannot_be_minimized}, \code{heuristic_only}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Elbow(V)
}
