% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OrthFilt}
\alias{OrthFilt}
\title{Daubechies filter bank taps and their orthonormality identities}
\usage{
OrthFilt(order = 4)
}
\arguments{
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{dec_lo}, \code{dec_hi}, \code{rec_lo}, \code{rec_hi}, \code{order}, \code{length}, \code{vanishing_moments}, \code{sum_lo}, \code{norm_lo}, \code{max_shift_inner_product}, \code{method}.
}
\keyword{internal}
\examples{
OrthFilt()
}
