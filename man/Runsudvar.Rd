% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Runsudvar}
\alias{Runsudvar}
\title{Runs up-and-down moments -- Sec. 3.4, p. 93 (Levene 1952)}
\usage{
Runsudvar(n, r = NULL, alpha = 0.05)
}
\description{
Runs up-and-down moments -- Sec. 3.4, p. 93 (Levene 1952)
}
\value{
A list with \code{mean}, \code{var}, \code{sd}, \code{z_left}, \code{z_right}, \code{p_value}, \code{zcrit}, \code{n}.
}
\keyword{internal}
\examples{
Runsudvar(n = 5L)
}
