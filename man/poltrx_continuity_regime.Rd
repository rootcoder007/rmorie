% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_continuity_regime}
\alias{poltrx_continuity_regime}
\title{Continuity regime implied by a parameter rule}
\usage{
poltrx_continuity_regime(rule)
}
\arguments{
\item{rule}{One of \code{"m_squared"}, \code{"constant"},   \code{"linear"}.}
}
\description{
Names the consequence of a parameter rule for the realised draw.
}
\value{
A list with \code{rule}, \code{draws} and \code{reason}.
}
\keyword{internal}
\examples{
poltrx_continuity_regime("m_squared")
}
