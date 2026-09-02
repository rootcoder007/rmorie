% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_morrisM}
\alias{morie_morrisM}
\title{morie_morrisM}
\usage{
morie_morrisM(fun, k, r = 10, p = 4, seed = 0, bounds = NULL) 
}
\arguments{
\item{fun}{Accepted by the signature and not used anywhere in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{r}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10}.}
\item{p}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{4}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{bounds}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
Part of the morrisM_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{mu}, \code{mu_star}, \code{sigma}, \code{elementary_effects}, \code{n_runs}, \code{delta}, \code{n_levels}, \code{r}, \code{p}, \code{k}, \code{method}.
}
\keyword{internal}
