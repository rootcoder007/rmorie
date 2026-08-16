% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Expmc}
\alias{Expmc}
\title{Exponential mechanism (alias)}
\usage{
Expmc(candidates, utility, epsilon = 1, sensitivity = 1, seed = NULL)
}
\description{
Alias of \code{morie_dp_exponential_mechanism}: selects candidate r with probability proportional to \eqn{\exp(\epsilon u(D, r) / (2 \Delta u))}. The generated stub for module expmc described exactly the mechanism already shipped, so this is an alias.
}
\value{
The value of \code{morie_dp_exponential_mechanism}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Expmc(V, V)
}
