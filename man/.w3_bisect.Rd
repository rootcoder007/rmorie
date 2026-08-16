% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_bisect}
\alias{.w3_bisect}
\title{Root of f on a bracketing interval, by plain bisection. A fixed}
\usage{
.w3_bisect(f, lo, hi, iters = 200L)
}
\arguments{
\item{f}{See Usage.}
\item{lo}{See Usage.}
\item{hi}{See Usage.}
\item{iters}{Defaults to \code{200L}.}
}
\description{
iteration count rather than a tolerance loop: the two arms then take exactly the same number of steps and end at exactly the same point, which a tolerance test cannot guarantee.
}
\value{
A numeric value.
}
\keyword{internal}
