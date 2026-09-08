% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{pair_cost}
\alias{pair_cost}
\title{pair_cost}
\usage{
pair_cost(n, mode = "cross-encoder") 
}
\arguments{
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{mode}{One of \code{"bi-encoder"}, \code{"cross-encoder"}. Defaults to \code{"cross-encoder"}.}
}
\description{
A step of the sbert_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{forward_passes}, \code{cross_encoder}, \code{bi_encoder}, \code{speedup}, \code{n}, \code{note}.
}
\keyword{internal}
\examples{
pair_cost(n = 5L)
}
