% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_hermite_coefficients}
\alias{.schab_hermite_coefficients}
\title{B_p = integral g(x) eta_p(x) f(x) dx, eq (5.65), by Gauss-Hermite}
\usage{
.schab_hermite_coefficients(g, degree, n_quad = NULL)
}
\arguments{
\item{g}{See Usage.}
\item{degree}{Numeric; combined arithmetically in the body.}
\item{n_quad}{Optional; may be \code{NULL}. Passed to \code{.schab_gauss_hermite}.}
}
\description{
quadrature -- exact for polynomial g, so the Example 5.12 identities come out exactly.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
