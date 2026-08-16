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
\item{lags}{A vector; indexed elementwise.}
\item{ghat}{A vector; indexed elementwise.}
\item{counts}{Passed to \code{.schab_gauss_newton}.}
\item{model}{Passed to \code{.schab_gauss_newton}. Defaults to \code{"exponential"}.}
\item{kind}{One of \code{"ols"}, \code{"wls"}. Defaults to \code{"wls"}.}
}
\description{
A step of the schab_fit_shared implementation. Called by \code{spols}, \code{spwls}. See the file header for the source the module follows.
}
\value{
A list with \code{nugget}, \code{partial_sill}, \code{range}, \code{objective}, \code{converged}.
}
\keyword{internal}
