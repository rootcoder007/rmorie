% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{changepoint_move_probabilities}
\alias{changepoint_move_probabilities}
\title{changepoint_move_probabilities}
\usage{
changepoint_move_probabilities(lam, k_max, cap = 0.9)
}
\arguments{
\item{lam}{Numeric; combined arithmetically in the body.}
\item{k_max}{Numeric; combined arithmetically in the body.}
\item{cap}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}. See the file header for the source the module follows.
}
\value{
A list with \code{eta}, \code{pi}, \code{b}, \code{d}, \code{c}.
}
\keyword{internal}
\examples{
changepoint_move_probabilities(lam = 5L, k_max = 5L)
}
