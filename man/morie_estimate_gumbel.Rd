% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_estimate_gumbel}
\alias{morie_estimate_gumbel}
\title{morie_estimate_gumbel}
\usage{
morie_estimate_gumbel(m, n, letter_freqs, match = 5, mismatch = -4, matrix = NULL, alphabet = "ACGT", n_sim = 200, seed = 0, quantiles = c(0.2, 0.9))
}
\arguments{
\item{m}{See Usage.}
\item{n}{See Usage.}
\item{letter_freqs}{See Usage.}
\item{match}{Defaults to \code{5}.}
\item{mismatch}{Defaults to \code{-4}.}
\item{matrix}{Defaults to \code{NULL}.}
\item{alphabet}{Defaults to \code{"ACGT"}.}
\item{n_sim}{Defaults to \code{200}.}
\item{seed}{Defaults to \code{0}.}
\item{quantiles}{Defaults to \code{c(0.2, 0.9)}.}
}
\description{
Part of the blstn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{lam}, \code{K}, \code{scores}.
}
\keyword{internal}
