% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OaeFeat}
\alias{OaeFeat}
\title{OaeFeat: otoacoustic emission band analysis}
\usage{
OaeFeat(oae, fs, noise_floor = NULL, bands = NULL)
}
\arguments{
\item{oae}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{noise_floor}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{bands}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
