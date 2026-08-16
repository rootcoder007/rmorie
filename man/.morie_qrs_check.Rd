% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_check}
\alias{.morie_qrs_check}
\title{.morie_qrs_check}
\usage{
.morie_qrs_check(x, least = 1L, what = "signal")
}
\arguments{
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{least}{Defaults to \code{1L}.}
\item{what}{Defaults to \code{"signal"}.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{ApneaEdr}, \code{CPulseFeat}, \code{DicNotch} and 29 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_qrs_check(V)
}
