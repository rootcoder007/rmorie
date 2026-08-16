% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_hermite_e}
\alias{.schab_hermite_e}
\title{.schab_hermite_e}
\usage{
.schab_hermite_e(x, degree)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{degree}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the schab_hermite_shared implementation. Called by \code{.schab_hermite_orthonormal}, \code{.schab_indicator_coefficients}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.schab_hermite_e(x = c(1, 2, 3, 4, 5, 6, 7, 8), degree = 5L)
}
