% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PoleZero}
\alias{PoleZero}
\title{Eqs (3.67), (3.69): zeros are the roots of the numerator, poles the}
\usage{
PoleZero(b, a = NULL)
}
\arguments{
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{a}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
roots of the denominator.  The denominator is in the book\'s normalized form, so pass `a` WITHOUT the leading 1.
}
\value{
A list with \code{zeros}, \code{poles}, \code{n_zeros}, \code{n_poles}, \code{stable}, \code{minimum_phase}, \code{zeros_on_unit_circle}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PoleZero(V)
}
