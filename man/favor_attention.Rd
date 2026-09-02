% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{favor_attention}
\alias{favor_attention}
\title{favor_attention}
\usage{
favor_attention(Q, K, V, n_features = 128L, seed = 0L, kind = "positive", orthogonal = TRUE, 
    causal = FALSE) 
}
\arguments{
\item{Q}{A matrix; passed to \code{as.matrix}.}
\item{K}{A matrix; passed to \code{as.matrix}.}
\item{V}{A matrix; passed to \code{as.matrix}.}
\item{n_features}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{128L}.}
\item{seed}{Passed to \code{draw_projections}. Defaults to \code{0L}.}
\item{kind}{Carried through into a list the body builds. Defaults to \code{"positive"}.}
\item{orthogonal}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{causal}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the perfat_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{output}, \code{n_features}, \code{kind}, \code{orthogonal}, \code{causal}, \code{L}, \code{d}, \code{d_v}, \code{method}.
}
\keyword{internal}
\examples{
favor_attention(Q = c(1, 2, 3, 4, 5, 6, 7, 8), K = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
