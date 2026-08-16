% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dispersion_trend}
\alias{dispersion_trend}
\title{Fit the dispersion trend (eq. 6)}
\usage{
dispersion_trend(mu_bar, disp, max_iter = 10L, tol = 1e-06)
}
\arguments{
\item{mu_bar}{Per-gene mean of normalised counts.}
\item{disp}{Gene-wise dispersion estimates.}
\item{max_iter}{Maximum iterations.}
\item{tol}{Convergence tolerance.}
}
\description{
Gamma-family GLM with an identity link, iterating with genes whose dispersion/fit ratio falls outside \code{[1e-4, 15]} excluded.
}
\value{
A list with \code{a1}, \code{a0} and \code{fitted}.
}
\keyword{internal}
