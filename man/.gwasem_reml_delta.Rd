% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_reml_delta}
\alias{.gwasem_reml_delta}
\title{.gwasem_reml_delta}
\usage{
.gwasem_reml_delta(y, X, evals, evecs, ml = FALSE, lo = -10, hi = 10, n_grid = 100L, refine = 60L)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{evals}{See Usage.}
\item{evecs}{See Usage.}
\item{ml}{Defaults to \code{FALSE}.}
\item{lo}{Defaults to \code{-10}.}
\item{hi}{Defaults to \code{10}.}
\item{n_grid}{Defaults to \code{100L}.}
\item{refine}{Defaults to \code{60L}.}
}
\description{
Part of the gwasem_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{delta}, \code{sigma_a2}, \code{sigma_e2}, \code{loglik}.
}
\keyword{internal}
