% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_fourier_terms}
\alias{morie_prphet_fourier_terms}
\title{morie_prphet_fourier_terms}
\usage{
morie_prphet_fourier_terms(t, period, order)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{period}{Numeric; combined arithmetically in the body.}
\item{order}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the prphet_native implementation. Called by \code{morie_prphet_design}, \code{prophe_additive_components}. See the file header for the source the module follows.
}
\value{
The value of \code{rows}, as built in the body.
}
\keyword{internal}
\examples{
morie_prphet_fourier_terms(t = c(1, 2, 3, 4, 5, 6, 7, 8), period = 5L, order = 5L)
}
