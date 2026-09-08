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
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_filters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{26}.}
\item{n_coeffs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{13}.}
\item{fmin}{The body requires: need 0 <= fmin < fmax <= fs/2. Defaults to \code{0}.}
\item{fmax}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_cep implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mfcc}, \code{filterbank_energies}, \code{log_energies}, \code{edges}, \code{n_filters}, \code{n_coeffs}, \code{fs}, \code{empty_filters}, \code{c0_is_energy}, \code{method}.
}
\keyword{internal}
\examples{
Mfcc(x = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
