% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{effect_curve}
\alias{effect_curve}
\title{Stage 2: regress the pseudo-outcome on the treatment}
\usage{
effect_curve(xi, A, grid, fit = "kernel", bandwidth = NULL, n_folds = 5)
}
\arguments{
\item{xi}{Pseudo-outcome vector.}
\item{A}{Treatment vector.}
\item{grid}{Grid of treatment values to evaluate the curve at.}
\item{fit}{One of \code{"kernel"}, \code{"locallinear"},   \code{"polynomial"}.}
\item{bandwidth}{Optional bandwidth; otherwise chosen by   cross-validation.}
\item{n_folds}{Number of CV folds.}
}
\description{
\code{kernel} is the Nadaraya-Watson estimator the paper analyses, \code{locallinear} the local-linear version that is less biased at the boundary, and \code{polynomial} a global cubic for when the curve is genuinely smooth and the sample is small.
}
\value{
A list with the curve and \code{info} (bandwidth, coef).
}
\keyword{internal}
\examples{
effect_curve(xi = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), grid = c(1, 2, 3, 4, 5, 6, 7, 8))
}
