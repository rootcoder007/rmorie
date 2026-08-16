% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{favor_features}
\alias{favor_features}
\title{favor_features}
\usage{
favor_features(X, omegas, kind = "positive", eps = 1e-06)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{omegas}{A matrix; passed to \code{nrow}.}
\item{kind}{One of \code{"positive"}, \code{"trig"}. Defaults to \code{"positive"}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.}
}
\description{
A step of the perfat_native implementation. Called by \code{favor_attention}, \code{kernel_estimate}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
favor_features(M, M)
}
