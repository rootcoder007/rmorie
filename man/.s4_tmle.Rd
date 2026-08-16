% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_tmle}
\alias{.s4_tmle}
\title{# One TMLE pass for a binary point treatment. W carries its intercept}
\usage{
.s4_tmle(y, D, W, gbound = 0.025)
}
\arguments{
\item{y}{See Usage.}
\item{D}{See Usage.}
\item{W}{See Usage.}
\item{gbound}{Defaults to \code{0.025}.}
}
\description{
Part of the s04core implementation; see the file header for the source it follows.
}
\value{
A list with \code{psi}, \code{se}, \code{eps}, \code{g}, \code{H}, \code{Q1}, \code{Q0}, \code{ic}, \code{n}.
}
\keyword{internal}
