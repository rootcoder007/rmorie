% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ValvePcg}
\alias{ValvePcg}
\title{ValvePcg: prosthetic heart valve evaluation via PCG AR spectrum}
\usage{
ValvePcg(pcg, fs, n_peaks = 3, order = NULL)
}
\arguments{
\item{pcg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{n_peaks}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{order}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
