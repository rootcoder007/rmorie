% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfImpulse}
\alias{MfImpulse}
\title{Eqs (4.49), (4.56): h(t) = K x(t0 - t), reversed and delayed.  The}
\usage{
MfImpulse(x, t0 = NULL, gain = 1, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{t0}{Defaults to \code{NULL}.}
\item{gain}{Defaults to \code{1}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
delay must be at least the reference duration or the filter is not causal; the book notes an N-point DFT of an N-sample template gives only N-1, one sample short.
}
\value{
A list with \code{h}, \code{t0}, \code{shift_samples}, \code{gain}, \code{causal}, \code{reversed}, \code{n_reference}, \code{method}.
}
\keyword{internal}
