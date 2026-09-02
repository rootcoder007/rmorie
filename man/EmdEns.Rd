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
\item{x}{Passed to \code{.tf_need}.}
\item{n_ensembles}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
\item{noise_std}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.2}.}
\item{max_imfs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{seed}{Passed to \code{.tf_lcg_seed}. Defaults to \code{0}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{n_ensembles}, \code{noise_std}, \code{seed}, \code{reconstruction_error}, \code{energy_per_imf}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
EmdEns(V)
}
