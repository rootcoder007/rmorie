% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EmdEns}
\alias{EmdEns}
\title{Ensemble EMD, Sec 9.4.1 eq (9.13)}
\usage{
EmdEns(x, n_ensembles = 20, noise_std = 0.2, max_imfs = 8, seed = 0)
}
\arguments{
\item{x}{See Usage.}
\item{n_ensembles}{Defaults to \code{20}.}
\item{noise_std}{Defaults to \code{0.2}.}
\item{max_imfs}{Defaults to \code{8}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{n_ensembles}, \code{noise_std}, \code{seed}, \code{reconstruction_error}, \code{energy_per_imf}, \code{method}.
}
\keyword{internal}
