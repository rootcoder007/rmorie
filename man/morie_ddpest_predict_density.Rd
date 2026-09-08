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
\examples{
G <- morie_ddpest_single_atoms(xs = c(0, 1), alpha = 1, K = 5,
                               weight_fn = function(x, h)
                                 exp(-abs(h - 2 * x)))$G
r <- morie_ddpest_predict_density(G, 0, grid = seq(-1, 5, by = 0.5),
                                  kernel = function(y, a) dnorm(y - a))
str(r, max.level = 1)
}
