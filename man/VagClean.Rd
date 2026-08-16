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
\item{vag}{See Usage.}
\item{emg_ref}{See Usage.}
\item{fs}{See Usage.}
\item{n_taps}{Defaults to \code{8}.}
\item{mu}{Defaults to \code{0.05}.}
\item{alpha}{Defaults to \code{0.02}.}
\item{adaptive_mu}{Defaults to \code{TRUE}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{cleaned}, \code{artifact_estimate}, \code{weights}, \code{rms_before}, \code{rms_after}, \code{artifact_reduction_db}, \code{mu_trace}, \code{n_taps}, \code{alpha}, \code{adaptive_mu}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
