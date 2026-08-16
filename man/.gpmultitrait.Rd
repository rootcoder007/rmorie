% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpmultitrait}
\alias{.gpmultitrait}
\title{.gpmultitrait}
\usage{
.gpmultitrait(Y, Z, G, Sigma_T, R_T, X = NULL)
}
\arguments{
\item{Y}{Passed to \code{.gpmat}.}
\item{Z}{Passed to \code{.gpkron}.}
\item{G}{Passed to \code{.gpkron}.}
\item{Sigma_T}{Passed to \code{.gpkron}.}
\item{R_T}{Passed to \code{.gpkron}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.gpmat}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm026}, \code{Msm028}. See the file header for the source the module follows.
}
\value{
A list with \code{mu}, \code{beta}, \code{b}, \code{b_by_line}.
}
\keyword{internal}
