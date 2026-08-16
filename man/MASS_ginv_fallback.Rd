% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MASS_ginv_fallback}
\alias{MASS_ginv_fallback}
\title{Pseudo-inverse fallback for a singular observed information}
\usage{
MASS_ginv_fallback(H)
}
\arguments{
\item{H}{See Usage.}
}
\description{
A step of the evt_coles implementation. Called by \code{morie_evt_gev_mle}, \code{morie_evt_gpd_mle}. See the file header for the source the module follows.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
MASS_ginv_fallback(H = 0.5)
}
