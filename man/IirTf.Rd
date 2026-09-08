% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IirTf}
\alias{IirTf}
\title{Eq (3.67).  The leading 1 of the denominator is part of the equation,}
\usage{
IirTf(b_k, a_k, z, N = NULL, M = NULL) 
}
\arguments{
\item{b_k}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{a_k}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Passed to \code{.morie_rg_polyz}.}
\item{N}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{M}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
so a_k is a_1..a_M WITHOUT it; passing a vector that already carries a leading 1 silently doubles the order, so the denominator actually used comes back for checking.
}
\value{
A list with \code{H}, \code{z}, \code{numerator}, \code{denominator}, \code{N}, \code{M}, \code{leading_one_is_implicit}, \code{method}.
}
\keyword{internal}
\examples{
IirTf(b_k = c(1, 2, 3, 4, 5, 6, 7, 8), a_k = c(1, 2, 3, 4, 5, 6, 7, 8),
  z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
