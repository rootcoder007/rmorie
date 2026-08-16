% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LpcSynth}
\alias{LpcSynth}
\title{Inverting eq (7.18): y(n) = G e(n) - sum a_k y(n-k).  The minus}
\usage{
LpcSynth(a, excitation, gain = 1, initial = NULL)
}
\arguments{
\item{a}{See Usage.}
\item{excitation}{See Usage.}
\item{gain}{Defaults to \code{1}.}
\item{initial}{Defaults to \code{NULL}.}
}
\description{
follows from A(z) = 1 + sum a_k z^-k.  Coefficients from the other convention give a filter with different poles, usually unstable, so divergence is reported rather than a wall of infinities returned.
}
\value{
A list with \code{y}, \code{n}, \code{order}, \code{gain}, \code{diverged}, \code{sign_convention}, \code{method}.
}
\keyword{internal}
