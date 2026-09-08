% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_mlm_loss}
\alias{morie_kamath_mlm_loss}
\title{morie_kamath_mlm_loss}
\usage{
morie_kamath_mlm_loss(x, M_x) 
}
\arguments{
\item{M_x}{0-based scored positions, no duplicates.}

\item{x}{Argument \code{x}; see Usage.}
}
\description{
morie_kamath_mlm_loss
}
\value{
A list with \code{estimate}, \code{per_position}, \code{positions_scored}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
p <- c(0.5, 1, 0.25, 1)
morie_kamath_mlm_loss(p, c(0, 2))
}
