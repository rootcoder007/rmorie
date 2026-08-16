% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2sinkhorn}
\alias{.b2sinkhorn}
\title{Sinkhorn scaling, FIXED iteration count.  K = exp(-C/eps), i.e}
\usage{
.b2sinkhorn(a, b, C, epsilon, max_iter = 200L, trace = FALSE)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
\item{C}{See Usage.}
\item{epsilon}{See Usage.}
\item{max_iter}{Defaults to \code{200L}.}
\item{trace}{Defaults to \code{FALSE}.}
}
\description{
Cuturi\\\'s exp(-lambda M) with lambda = 1/eps; updates u <- a/(K v) then v <- b/(K\\\' u), started from v = 1.
}
\value{
A list with \code{T}, \code{u}, \code{v}, \code{a}, \code{b}, \code{trace}.
}
\keyword{internal}
