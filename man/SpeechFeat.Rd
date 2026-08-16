% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpeechFeat}
\alias{SpeechFeat}
\title{SpeechFeat: speech formants and autocorrelation pitch}
\usage{
SpeechFeat(speech, fs, order = NULL, n_formants = 4, f0_range = c(60, 400))
}
\arguments{
\item{speech}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{order}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{n_formants}{Passed to \code{.bsapeaks}. Defaults to \code{4}.}
\item{f0_range}{A vector; indexed elementwise. Defaults to \code{c(60, 400)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{formants_hz}, \code{formant_bandwidths_hz}, \code{formant_powers}, \code{f0_hz}, \code{pitch_period_ms}, \code{voiced}, \code{voicing_strength}, \code{voicing_threshold}, \code{zero_crossing_rate}, \code{order}, \code{ar_coeffs}, \code{prediction_error}, \code{freq_hz}, \code{lpc_psd}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
