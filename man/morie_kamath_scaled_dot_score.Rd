% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_scaled_dot_score}
\alias{morie_kamath_scaled_dot_score}
\title{morie_kamath_scaled_dot_score}
\usage{
morie_kamath_scaled_dot_score(q, k, d_k = NULL)
}
\arguments{
\item{k}{Key.}
\item{d_k}{Optional dimension pin.}
}
\description{
morie_kamath_scaled_dot_score
}
\value{
A list with \code{estimate}, \code{d_k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_scaled_dot_score(V, V)
}
