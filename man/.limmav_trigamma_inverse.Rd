% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_trigamma_inverse}
\alias{.limmav_trigamma_inverse}
\title{.limmav_trigamma_inverse}
\usage{
.limmav_trigamma_inverse(x, tol = 1e-08, max_iter = 60)
}
\arguments{
\item{x}{Numeric; passed to \code{sqrt}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60}.}
}
\description{
A step of the limmav_native implementation. Called by \code{.limmav_ebayes}. See the file header for the source the module follows.
}
\value{
The value of \code{y}, as built in the body.
}
\keyword{internal}
\examples{
.limmav_trigamma_inverse(x = 5L)
}
