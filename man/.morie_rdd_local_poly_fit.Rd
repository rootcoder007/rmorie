% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_local_poly_fit}
\alias{.morie_rdd_local_poly_fit}
\title{.morie_rdd_local_poly_fit}
\usage{
.morie_rdd_local_poly_fit(x, y, x0, h, p = 1, kernel = "triangular")
}
\description{
.morie_rdd_local_poly_fit
}
\value{
A list with \code{beta}, \code{se}, \code{n}, \code{fit_value}.
}
\keyword{internal}
\examples{
.morie_rdd_local_poly_fit(x = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), y = c(1, 2, 3, 4, 5, 6, 7, 8), x0 = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
