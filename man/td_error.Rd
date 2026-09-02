% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{td_error}
\alias{td_error}
\title{Temporal-difference residual}
\usage{
td_error(q_sa, target) 
}
\arguments{
\item{q_sa}{Scalar current Q(s, a).}
\item{target}{Scalar TD target.}
}
\description{
Returns \code{target - q_sa}, the scalar residual that the squared-error Bellman loss minimises.
}
\value{
Scalar difference \code{target - q_sa}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
td_error(V, V)
}
