% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_slice_chain}
\alias{morie_slice_chain}
\title{Univariate slice-sampling chain}
\usage{
morie_slice_chain(logf, x0, n = 2000L, w = 1, burn = 0L, seed = 1L, lower = -Inf, upper = Inf, thin = 1L)
}
\arguments{
\item{logf}{Log unnormalised density.}
\item{x0}{Starting value.}
\item{n}{Number of draws to keep.}
\item{w}{Slice width.}
\item{burn}{Iterations to discard before recording.}
\item{seed}{Seed for the shared generator.}
\item{lower,upper}{Support bounds.}
\item{thin}{Thinning factor: keep every \code{thin}-th draw.}
}
\description{
Runs \code{burn + n * thin} slice transitions and records every \code{thin}-th draw from \code{burn} onwards.
}
\value{
List with \code{draws}, \code{n_eval}, \code{w},   \code{evals_per_draw}.
}
\keyword{internal}
