% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest_predict_density}
\alias{morie_ddpest_predict_density}
\title{The density at x: sum_h w_h(x) k(y | theta_h(x))}
\usage{
morie_ddpest_predict_density(G, x, grid, kernel)
}
\arguments{
\item{G}{A collection.}
\item{x}{Covariate value.}
\item{grid}{Grid of evaluation points y.}
\item{kernel}{Function (y, atom) returning a non-negative density   value.}
}
\description{
The density at x: sum_h w_h(x) k(y | theta_h(x))
}
\value{
Named list with estimate, density, grid, x, n_components,   method, note.
}
\keyword{internal}
