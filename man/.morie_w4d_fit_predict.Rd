% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_fit_predict}
\alias{.morie_w4d_fit_predict}
\title{.morie_w4d_fit_predict}
\usage{
.morie_w4d_fit_predict(A, t)
}
\arguments{
\item{A}{See Usage.}
\item{t}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_self_supervised}. See the file header for the source the module follows.
}
\value{
A list with \code{theta}, \code{fitted}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_w4d_fit_predict(V, V)
}
