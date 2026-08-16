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
\item{n_imfs}{Defaults to \code{6}.}
\item{tol}{Defaults to \code{0.05}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{features}, \code{dominant_imf}, \code{dominant_freq}, \code{method}.
}
\keyword{internal}
