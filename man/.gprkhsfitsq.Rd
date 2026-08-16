% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gprkhsfitsq}
\alias{.gprkhsfitsq}
\title{.gprkhsfitsq}
\usage{
.gprkhsfitsq(K, y, lam = 1)
}
\arguments{
\item{K}{Passed to \code{.gpmat}.}
\item{y}{Passed to \code{.gpflat}.}
\item{lam}{Defaults to \code{1}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm128}. See the file header for the source the module follows.
}
\value{
A list with \code{eta0}, \code{beta}, \code{fitted}, \code{residuals}, \code{loss}, \code{penalty}, \code{objective}.
}
\keyword{internal}
