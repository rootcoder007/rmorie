% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_select_bandwidth}
\alias{.schab_select_bandwidth}
\title{.schab_select_bandwidth}
\usage{
.schab_select_bandwidth(y, X, coords, kernel = "gaussian", criterion = "cv", adaptive = FALSE, bounds = NULL, tol = 1e-04)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{coords}{See Usage.}
\item{kernel}{Defaults to \code{"gaussian"}.}
\item{criterion}{Defaults to \code{"cv"}.}
\item{adaptive}{Defaults to \code{FALSE}.}
\item{bounds}{Defaults to \code{NULL}.}
\item{tol}{Defaults to \code{1e-04}.}
}
\description{
Part of the schab_gwr_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{bandwidth}, \code{score}, \code{criterion}, \code{bounds}, \code{adaptive}.
}
\keyword{internal}
