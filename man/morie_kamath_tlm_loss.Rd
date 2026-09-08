% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_tlm_loss}
\alias{morie_kamath_tlm_loss}
\title{morie_kamath_tlm_loss}
\usage{
morie_kamath_tlm_loss(x, y, M_x, M_y) 
}
\arguments{
\item{y}{Target-side probabilities.}
\item{M_y}{Target mask.}

\item{x}{Argument \code{x}; see Usage.}

\item{M_x}{Argument \code{M_x}; see Usage.}
}
\description{
morie_kamath_tlm_loss
}
\value{
A list with \code{estimate}, \code{source_loss}, \code{target_loss}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_tlm_loss(c(0.5, 1), 0.25, 0, 0)
}
