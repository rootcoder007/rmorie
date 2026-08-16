% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_lsa}
\alias{morie_lsa}
\title{morie_lsa}
\usage{
morie_lsa(X, k_dim = NULL, how = "log_entropy", query = NULL, top_k = 5L)
}
\arguments{
\item{X}{See Usage.}
\item{k_dim}{Defaults to \code{NULL}.}
\item{how}{Defaults to \code{"log_entropy"}.}
\item{query}{Defaults to \code{NULL}.}
\item{top_k}{Defaults to \code{5L}.}
}
\description{
A step of the lsa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{lsa_decompose}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_lsa(M)
}
