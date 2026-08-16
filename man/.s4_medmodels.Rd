% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_medmodels}
\alias{.s4_medmodels}
\title{# Outcome model Y = th0 + th1 a + th2 m + th3 a m + th4\\\'c and}
\usage{
.s4_medmodels(Y, A, M, Cc = NULL)
}
\arguments{
\item{Y}{See Usage.}
\item{A}{See Usage.}
\item{M}{See Usage.}
\item{Cc}{Defaults to \code{NULL}.}
}
\description{
mediator # model M = b0 + b1 a + b2\\\'c; cbar is where the decomposition is read.
}
\value{
A list with \code{theta}, \code{beta}, \code{cbar}.
}
\keyword{internal}
