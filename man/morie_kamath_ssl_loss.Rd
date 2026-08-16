% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_ssl_loss}
\alias{morie_kamath_ssl_loss}
\title{The Kamath Ch 2 pretraining loss family (Eq 2.20-2.33)}
\usage{
morie_kamath_ssl_loss(L_PTi, lambda_i = NULL)
}
\arguments{
\item{L_PTi}{Pretext losses.}
\item{lambda_i}{Optional weights.}
}
\description{
The Kamath Ch 2 pretraining loss family (Eq 2.20-2.33)
}
\value{
A list with \code{estimate}, \code{components}, \code{lambdas}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_ssl_loss(V)
}
