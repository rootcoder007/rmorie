% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tqang}
\alias{morie_tqang}
\title{Quantise angles to 2^bits uniform sectors}
\usage{
morie_tqang(theta, bits = 4)
}
\arguments{
\item{theta}{Vector of angles in radians.}
\item{bits}{Bits per code.}
}
\description{
Quantise angles to 2^bits uniform sectors
}
\value{
A list with \code{estimate}, \code{indices}, \code{values},   \code{errors}, \code{mse}, \code{max_abs_error}, \code{delta},   \code{half_delta}, \code{mse_bound}, \code{bits}, \code{levels},   \code{method}.
}
\keyword{internal}
\examples{
q <- morie_tqang(c(0.1, 0.5, 1.0, -2.0), bits = 8)
is.list(q) || is.numeric(q)
}
