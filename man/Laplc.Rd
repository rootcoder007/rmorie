% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Laplc}
\alias{Laplc}
\title{Laplace mechanism (alias)}
\usage{
Laplc(y, sensitivity = 1, epsilon = 1, seed = NULL)
}
\description{
Alias of \code{morie_dp_laplace_mechanism}: releases \eqn{f(D) + Lap(\Delta f / \epsilon)}. The generated stub for module laplc described exactly the mechanism already shipped, so this is an alias, not a second implementation.
}
\value{
The value of \code{morie_dp_laplace_mechanism}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Laplc(V)
}
