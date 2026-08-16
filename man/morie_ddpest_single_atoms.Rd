% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest_single_atoms}
\alias{morie_ddpest_single_atoms}
\title{Common atoms, weights moving with x}
\usage{
morie_ddpest_single_atoms(xs, alpha, K, weight_fn, atom_sampler = NULL, seed = 0)
}
\arguments{
\item{xs}{Covariate values.}
\item{alpha}{Concentration parameter; not consumed here but kept   on the signature to mirror the Python arm.}
\item{K}{Number of atoms.}
\item{weight_fn}{Function (x, h) returning an unnormalised weight.}
\item{atom_sampler}{Optional function (rng, h) returning the h-th   shared atom; if NULL the atoms default to 0, 1, ..., K-1 as in   the Python arm.}
\item{seed}{Seed for the shared generator.}
}
\description{
weight_fn(x, h) returns an unnormalised weight; each G_x is renormalised so it remains a probability measure.
}
\value{
Named list with G, kind, atoms, K, note.
}
\keyword{internal}
\examples{
r <- morie_ddpest_single_atoms(xs = c(0, 1), alpha = 1, K = 5,
                               weight_fn = function(x, h)
                                 exp(-abs(h - 2 * x)))
str(r, max.level = 1)
}
