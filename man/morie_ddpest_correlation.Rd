% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest_correlation}
\alias{morie_ddpest_correlation}
\title{corr(G_x1(A), G_x2(A)) for a set A}
\usage{
morie_ddpest_correlation(G, x1, x2, region)
}
\arguments{
\item{G}{A collection.}
\item{x1}{First covariate value.}
\item{x2}{Second covariate value.}
\item{region}{Predicate on an atom.}
}
\description{
region is a predicate on an atom.
}
\value{
Named list with G_x1, G_x2, shared_mass, abs_difference,   identical, note.
}
\keyword{internal}
\examples{
G <- morie_ddpest_single_atoms(xs = c(0, 1), alpha = 1, K = 5,
                               weight_fn = function(x, h)
                                 exp(-abs(h - 2 * x)))$G
morie_ddpest_correlation(G, 0, 1, region = function(a) a <= 2)
}
