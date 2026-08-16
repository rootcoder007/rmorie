% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_norm_inter}
\alias{.morie_norm_inter}
\title{Boot:::norm.inter -- interpolation of order statistics on the normal}
\usage{
.morie_norm_inter(t, alpha)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{alpha}{A vector; its length is taken and its elements indexed.}
}
\description{
quantile scale (shared by percentile / basic / BCa).
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_norm_inter(V, V)
}
