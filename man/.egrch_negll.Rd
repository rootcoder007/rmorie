% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.egrch_negll}
\alias{.egrch_negll}
\title{Parameter naming for EGARCH is not standard across implementations:}
\usage{
.egrch_negll(p, r, n, EZ)
}
\arguments{
\item{p}{A vector; indexed elementwise.}
\item{r}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{numeric(...)}.}
\item{EZ}{Numeric; combined arithmetically in the body.}
}
\description{
arch (Python): log s2 = w + a(|z| - E|z|) + g z + b log s2 -- a = size rugarch (R): log s2 = w + a z + g(|z| - E|z|) + b log s2 -- a = sign We follow the first: alpha multiplies the centred magnitude (the size effect) and gamma the signed z (the sign / leverage effect), matching the Python sibling. This module used to compute that internally while reading alpha1/gamma1 out of a package using the opposite order, so the returned alpha and gamma swapped meaning depending on whether that package happened to be installed.
}
\value{
The value of \code{res}, as built in the body.
}
\keyword{internal}
\examples{
.egrch_negll(p = c(1, 2, 3, 4, 5, 6, 7, 8), r = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, EZ = c(1, 2, 3, 4, 5, 6, 7, 8))
}
