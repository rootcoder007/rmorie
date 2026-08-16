% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_gpdfit}
\alias{.s4_gpdfit}
\title{# Zhang-Stephens empirical-Bayes generalised Pareto fit. Fixed grid}
\usage{
.s4_gpdfit(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
of # 30 + floor(sqrt(N)) points weighted by the profile likelihood -- no # optimiser, so the arms cannot land on different local optima.
}
\value{
A list with \code{k}, \code{sigma}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s4_gpdfit(V)
}
