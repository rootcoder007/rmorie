% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_span_loss}
\alias{morie_kamath_span_loss}
\title{morie_kamath_span_loss}
\usage{
morie_kamath_span_loss(x, xhat, i, j)
}
\arguments{
\item{i,j}{0-based inclusive span bounds.}
}
\description{
morie_kamath_span_loss
}
\value{
A list with \code{estimate}, \code{span_length}, \code{per_position}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_span_loss(c(1e-09, 0.5, 0.25, 1e-09), "x", 1, 2)
}
