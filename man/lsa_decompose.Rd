% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lsa_decompose}
\alias{lsa_decompose}
\title{lsa_decompose}
\usage{
lsa_decompose(X, k_dim = NULL, how = "log_entropy")
}
\arguments{
\item{X}{See Usage.}
\item{k_dim}{Defaults to \code{NULL}.}
\item{how}{Defaults to \code{"log_entropy"}.}
}
\description{
A step of the lsa_native implementation. Called by \code{morie_lsa}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{T}, \code{S}, \code{D}, \code{k}, \code{full_rank}, \code{weighting}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
lsa_decompose(M)
}
