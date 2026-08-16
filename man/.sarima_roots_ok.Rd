% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_roots_ok}
\alias{.sarima_roots_ok}
\title{.sarima_roots_ok}
\usage{
.sarima_roots_ok(coefs, tol = 1.001)
}
\arguments{
\item{coefs}{A vector; its length is taken.}
\item{tol}{Passed to \code{<}. Defaults to \code{1.001}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.sarima_fit}, \code{.sarimax_fit}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sarima_roots_ok(V)
}
