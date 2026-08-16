% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mfcc}
\alias{Mfcc}
\title{Mfcc}
\usage{
Mfcc(x, fs, n_filters = 26, n_coeffs = 13, fmin = 0, fmax = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{n_filters}{Defaults to \code{26}.}
\item{n_coeffs}{Defaults to \code{13}.}
\item{fmin}{Defaults to \code{0}.}
\item{fmax}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_cep implementation; see the file header for the source it follows.
}
\value{
A list with \code{mfcc}, \code{filterbank_energies}, \code{log_energies}, \code{edges}, \code{n_filters}, \code{n_coeffs}, \code{fs}, \code{empty_filters}, \code{c0_is_energy}, \code{method}.
}
\keyword{internal}
\examples{
Mfcc(x = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
