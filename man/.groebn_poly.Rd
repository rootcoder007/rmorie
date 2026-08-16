% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_poly}
\alias{.groebn_poly}
\title{.groebn_poly}
\usage{
.groebn_poly(terms, nvars = NULL)
}
\arguments{
\item{terms}{A vector; its length is taken and its elements indexed.}
\item{nvars}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
Polynomial constructor ---------------------------------------------------------------------------
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_poly(V)
}
