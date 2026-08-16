% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{composition_budget}
\alias{composition_budget}
\title{Basic composition: k releases cost sum_i epsilon_i}
\usage{
composition_budget(epsilons)
}
\arguments{
\item{epsilons}{Vector of epsilons.}
}
\description{
Basic composition: k releases cost sum_i epsilon_i
}
\value{
A list with \code{total_epsilon}, \code{n_releases},   \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
composition_budget(V)
}
