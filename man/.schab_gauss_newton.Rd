% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gauss_newton}
\alias{.schab_gauss_newton}
\title{.schab_gauss_newton}
\usage{
.schab_gauss_newton(lags, ghat, counts, start, model = "exponential", kind = "wls", max_iter = 200L, tol = 1e-12, max_halvings = 40L)
}
\arguments{
\item{lags}{See Usage.}
\item{ghat}{See Usage.}
\item{counts}{See Usage.}
\item{start}{See Usage.}
\item{model}{Defaults to \code{"exponential"}.}
\item{kind}{Defaults to \code{"wls"}.}
\item{max_iter}{Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-12}.}
\item{max_halvings}{Defaults to \code{40L}.}
}
\description{
Part of the schab_gn_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{theta}, \code{objective}, \code{converged}, \code{iterations}.
}
\keyword{internal}
