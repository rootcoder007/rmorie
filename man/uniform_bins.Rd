% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{uniform_bins}
\alias{uniform_bins}
\title{uniform_bins}
\usage{
uniform_bins(lo = -15, hi = 15, n_bins = 4096L)
}
\arguments{
\item{lo}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{-15}.}
\item{hi}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{15}.}
\item{n_bins}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4096L}.}
}
\description{
A step of the chronos_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{centers}, \code{edges}, \code{n_bins}, \code{scheme}, \code{range}.
}
\keyword{internal}
\examples{
uniform_bins()
}
