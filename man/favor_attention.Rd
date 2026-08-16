% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{favor_attention}
\alias{favor_attention}
\title{favor_attention}
\usage{
favor_attention(Q, K, V, n_features = 128L, seed = 0L, kind = "positive", orthogonal = TRUE, causal = FALSE)
}
\arguments{
\item{Q}{See Usage.}
\item{K}{See Usage.}
\item{V}{See Usage.}
\item{n_features}{Defaults to \code{128L}.}
\item{seed}{Defaults to \code{0L}.}
\item{kind}{Defaults to \code{"positive"}.}
\item{orthogonal}{Defaults to \code{TRUE}.}
\item{causal}{Defaults to \code{FALSE}.}
}
\description{
Part of the perfat_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{output}, \code{n_features}, \code{kind}, \code{orthogonal}, \code{causal}, \code{L}, \code{d}, \code{d_v}, \code{method}.
}
\keyword{internal}
\examples{
favor_attention(Q = c(1, 2, 3, 4, 5, 6, 7, 8), K = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
