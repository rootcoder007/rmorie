% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OsFilt}
\alias{OsFilt}
\title{OsFilt}
\usage{
OsFilt(x, window, kind = "median", alpha = 0, weights = NULL, order = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{window}{See Usage.}
\item{kind}{Defaults to \code{"median"}.}
\item{alpha}{Defaults to \code{0}.}
\item{weights}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_filt implementation; see the file header for the source it follows.
}
\value{
A list with \code{y}, \code{n}, \code{window}, \code{kind}, \code{alpha}, \code{trimmed_each_end}, \code{order}, \code{nonlinear}, \code{no_frequency_response}, \code{edges}, \code{method}.
}
\keyword{internal}
