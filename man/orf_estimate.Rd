% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{orf_estimate}
\alias{orf_estimate}
\title{One point estimate theta(x). residualize="local" fits the}
\usage{
orf_estimate(Y, T, X, W, x, trees, residualize = "local", ridge = 1e-08, leave_one_out = TRUE)
}
\arguments{
\item{Y}{A vector; its length is taken and its elements indexed.}
\item{T}{A vector; indexed elementwise.}
\item{X}{Passed to \code{.orfgrf_forest_weights}.}
\item{W}{See Usage.}
\item{x}{Passed to \code{.orfgrf_forest_weights}.}
\item{trees}{Passed to \code{.orfgrf_forest_weights}.}
\item{residualize}{The body requires: orfgrf: residualize must be local or global, got. Defaults to \code{"local"}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{leave_one_out}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
nuisances under the same kernel weights used for the second stage -- the ORF proposal. "global" fits them once on the whole sample, the "local centering" benchmark.
}
\value{
A list with \code{theta}, \code{den}, \code{w}.
}
\keyword{internal}
