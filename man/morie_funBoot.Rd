% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funBoot}
\alias{morie_funBoot}
\title{morie_funBoot}
\usage{
morie_funBoot(curves, statistic = NULL, alpha = 0.05, B = 500L, metric = "l2", smooth = 0, seed = 0)
}
\arguments{
\item{curves}{See Usage.}
\item{statistic}{Defaults to \code{NULL}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{B}{Defaults to \code{500L}.}
\item{metric}{Defaults to \code{"l2"}.}
\item{smooth}{Defaults to \code{0}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the funBoot_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{center}, \code{radius}, \code{estimate}, \code{distances}, \code{n_within}, \code{metric}, \code{alpha}, \code{B}, \code{seed}, \code{method}.
}
\keyword{internal}
