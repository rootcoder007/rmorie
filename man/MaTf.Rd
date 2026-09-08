% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MaTf}
\alias{MaTf}
\title{Eq (3.99): a polynomial in z^-1 with no poles away from the origin,}
\usage{
MaTf(b_k, z, N = NULL) 
}
\arguments{
\item{b_k}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Passed to \code{.morie_rg_polyz}.}
\item{N}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
so an FIR filter is stable whatever its coefficients.
}
\value{
A list with \code{H}, \code{z}, \code{b}, \code{N}, \code{dc_gain}, \code{always_stable}, \code{poles_only_at_the_origin}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MaTf(V, V)
}
