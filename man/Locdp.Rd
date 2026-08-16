% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Locdp}
\alias{Locdp}
\title{Local differential privacy via randomized response (alias)}
\usage{
Locdp(bit, epsilon = 1)
}
\description{
Alias of \code{Rrand}. The generated stub for module locdp described the local model of differential privacy (each user randomizes before collection, Kasiviswanathan et al. 2011); the canonical local-DP mechanism is Warner randomized response with flip probability \eqn{1/(1 + e^{\epsilon})}, already shipped as \code{Rrand}.
}
\value{
The value of \code{Rrand}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Locdp(V)
}
