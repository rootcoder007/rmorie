% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest_single_weights}
\alias{morie_ddpest_single_weights}
\title{Common weights, atoms moving with x}
\usage{
morie_ddpest_single_weights(xs, alpha, K, atom_fn, seed = 0) 
}
\arguments{
\item{xs}{Covariate values.}
\item{alpha}{Concentration parameter of the GEM.}
\item{K}{Truncation level of the stick-breaking.}
\item{atom_fn}{Function (x, h) returning the h-th atom at covariate   x; deterministic, no rng argument.}
\item{seed}{Seed for the shared generator.}
}
\description{
One stick-breaking draw is shared by every covariate value; the atoms are theta_h(x).
}
\value{
Named list with G, kind, weights, K, note.
}
\keyword{internal}
\examples{
r <- morie_ddpest_single_weights(xs = c(0, 1), alpha = 1, K = 5,
                                 atom_fn = function(x, h) h + 0.1 * x)
str(r, max.level = 1)
}
