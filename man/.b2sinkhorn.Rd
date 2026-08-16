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
\item{a}{Passed to \code{.b2close}.}
\item{b}{Passed to \code{.b2close}.}
\item{C}{Passed to \code{.b2mat}.}
\item{epsilon}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{trace}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
Cuturi\\'s exp(-lambda M) with lambda = 1/eps; updates u <- a/(K v) then v <- b/(K\\' u), started from v = 1.
}
\value{
A list with \code{T}, \code{u}, \code{v}, \code{a}, \code{b}, \code{trace}.
}
\keyword{internal}
