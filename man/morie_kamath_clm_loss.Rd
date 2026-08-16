% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_clm_loss}
\alias{morie_kamath_clm_loss}
\title{morie_kamath_clm_loss}
\usage{
morie_kamath_clm_loss(x)
}
\arguments{
\item{x}{Per-position probabilities of the true token.}
}
\description{
morie_kamath_clm_loss
}
\value{
A list with \code{estimate}, \code{per_position}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_clm_loss(c(0.5, 0.25))
}
