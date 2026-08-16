% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.evt_lmom}
\alias{.evt_lmom}
\title{.evt_lmom}
\usage{
.evt_lmom(x)
}
\arguments{
\item{x}{Passed to \code{.evt_pwm}.}
}
\description{
A step of the evt_native implementation. Called by \code{morie_evt_gev_lmoments}. See the file header for the source the module follows.
}
\value{
A list with \code{l1}, \code{l2}, \code{l3}, \code{t3}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.evt_lmom(V)
}
