% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_interpolation}
\alias{morie_burkov_interpolation}
\title{Linear interpolation of n-gram orders (Burkov Ch 2)}
\usage{
morie_burkov_interpolation(probs_by_order, lambdas)
}
\arguments{
\item{probs_by_order}{Probabilities.}
\item{lambdas}{Weights, sum 1.}
}
\description{
Linear interpolation of n-gram orders (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{probs}, \code{lambdas}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_interpolation(c(0.9, 0.1, 0.5), c(0.6, 0.3, 0.1))
}
