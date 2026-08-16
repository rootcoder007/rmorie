% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AtomTfd}
\alias{AtomTfd}
\title{Matching-pursuit TFD, eq (9.15) over the Gabor dictionary}
\usage{
AtomTfd(x, fs = 1, dictionary = "gabor", max_atoms = 8, nfreq = NULL, min_decay = 0.001)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{dictionary}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"gabor"}.}
\item{max_atoms}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{nfreq}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{min_decay}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tfd}, \code{times}, \code{freqs}, \code{atoms}, \code{n_atoms}, \code{decay}, \code{residual_energy}, \code{explained}, \code{peak_freq}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
AtomTfd(V)
}
