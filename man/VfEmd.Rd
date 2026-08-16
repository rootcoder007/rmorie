% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VfEmd}
\alias{VfEmd}
\title{IMF-based characterisation of ventricular fibrillation, Sec 8.16}
\usage{
VfEmd(ecg, fs = 250, n_imfs = 6, tol = 0.05)
}
\arguments{
\item{ecg}{Passed to \code{.tf_need}.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{250}.}
\item{n_imfs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{features}, \code{dominant_imf}, \code{dominant_freq}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
fs <- 250
tv <- seq(0, 2, by = 1 / fs)
vf <- sin(2 * pi * 5 * tv + 2 * sin(2 * pi * 0.5 * tv)) +
  0.1 * rnorm(length(tv))
str(VfEmd(vf, fs = fs), max.level = 1)
}
