% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_fit_semivariogram}
\alias{.schab_fit_semivariogram}
\title{.schab_fit_semivariogram}
\usage{
.schab_fit_semivariogram(lags, ghat, counts, model = "exponential", kind = "wls")
}
\arguments{
\item{lags}{See Usage.}
\item{ghat}{See Usage.}
\item{counts}{See Usage.}
\item{model}{Defaults to \code{"exponential"}.}
\item{kind}{Defaults to \code{"wls"}.}
}
\description{
Part of the schab_fit_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{nugget}, \code{partial_sill}, \code{range}, \code{objective}, \code{converged}.
}
\keyword{internal}
