% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gibbs_slice}
\alias{morie_gibbs_slice}
\title{Gibbs sweep with a slice update per coordinate}
\usage{
morie_gibbs_slice(log_conditionals, x0, n = 2000L, w = NULL, burn = 0L, seed = 1L, bounds = NULL)
}
\arguments{
\item{log_conditionals}{List of functions; element \code{k} takes a   candidate value and the current state vector and returns the log of   the unnormalised full conditional of coordinate \code{k}.}
\item{x0}{Starting state vector.}
\item{n}{Number of Gibbs sweeps to keep.}
\item{w}{Per-coordinate slice width: scalar, vector, or \code{NULL}   for the default \code{1.0} per coordinate.}
\item{burn}{Sweeps to discard.}
\item{seed}{Seed.}
\item{bounds}{Per-coordinate \code{list(c(lo, hi))} or \code{NULL}.}
}
\description{
Each coordinate is updated in turn through one slice transition on its full conditional. This is the Damien-Wakefield-Walker construction: a non-conjugate conditional is replaced by an auxiliary uniform draw.
}
\value{
RichResult-like list with \code{estimate}, \code{draws},   \code{mean}, \code{n_draws}, \code{n_eval}, \code{ess},   \code{method}.
}
\keyword{internal}
