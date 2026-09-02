% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_estimate_gumbel}
\alias{morie_estimate_gumbel}
\title{morie_estimate_gumbel}
\usage{
morie_estimate_gumbel(m, n, letter_freqs, match = 5, mismatch = -4, matrix = NULL, 
    alphabet = "ACGT", n_sim = 200, seed = 0, quantiles = c(0.2, 0.9)) 
}
\arguments{
\item{m}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{letter_freqs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{match}{Passed to \code{morie_msp_exact}. Defaults to \code{5}.}
\item{mismatch}{Passed to \code{morie_msp_exact}. Defaults to \code{-4}.}
\item{matrix}{Passed to \code{morie_msp_exact}.}
\item{alphabet}{Character; passed to \code{strsplit}. Defaults to \code{"ACGT"}.}
\item{n_sim}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
\item{quantiles}{A vector; indexed elementwise. Defaults to \code{c(0.2, 0.9)}.}
}
\description{
A step of the blstn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{lam}, \code{K}, \code{scores}.
}
\keyword{internal}
\examples{
morie_estimate_gumbel(m = 5L, n = 5L, letter_freqs = c(1, 2, 3, 4, 5, 6, 7, 8))
}
