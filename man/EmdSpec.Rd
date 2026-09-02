% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EmdSpec}
\alias{EmdSpec}
\title{EMD-based instantaneous-frequency spectrum, Sec 9.4 eqs (9.8)-(9.12)}
\usage{
EmdSpec(x, fs = 1, max_imfs = 8, nfreq = 32, tol = 0.05) 
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{max_imfs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{nfreq}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{spectrum}, \code{times}, \code{freqs}, \code{imfs}, \code{amplitude}, \code{inst_freq}, \code{marginal}, \code{n_imfs}, \code{peak_freq}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
EmdSpec(V)
}
