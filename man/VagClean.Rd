% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VagClean}
\alias{VagClean}
\title{VagClean: muscle-contraction artifact removal from VAG signals (LMS)}
\usage{
VagClean(vag, emg_ref, fs, n_taps = 8, mu = 0.05, alpha = 0.02, adaptive_mu = TRUE)
}
\arguments{
\item{vag}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{emg_ref}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_taps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{mu}{Numeric; combined arithmetically in the body. Defaults to \code{0.05}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.02}.}
\item{adaptive_mu}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{cleaned}, \code{artifact_estimate}, \code{weights}, \code{rms_before}, \code{rms_after}, \code{artifact_reduction_db}, \code{mu_trace}, \code{n_taps}, \code{alpha}, \code{adaptive_mu}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(9)
fs <- 500
n <- fs
emg <- rnorm(n)
vag <- 0.5 * emg + rnorm(n, 0, 0.3)
VagClean(vag, emg, fs = fs)
}
