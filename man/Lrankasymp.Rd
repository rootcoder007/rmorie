% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lrankasymp}
\alias{Lrankasymp}
\title{Chernoff-Savage null moments -- Theorem 7.3.8 / Corollary 7.3.1, p. 285}
\usage{
Lrankasymp(j, jprime, lam, n, nodes = 2001) 
}
\description{
Chernoff-Savage null moments -- Theorem 7.3.8 / Corollary 7.3.1, p. 285
}
\arguments{
\item{j}{Argument \code{j}; see Usage.}
\item{jprime}{Argument \code{jprime}; see Usage.}
\item{lam}{Argument \code{lam}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{nodes}{Argument \code{nodes}; see Usage.}
}
\value{
A list with \code{mean}, \code{var}, \code{sd}, \code{integral}, \code{lam}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Lrankasymp(j = function(u) qnorm(u), jprime = function(u) 1 / dnorm(qnorm(u)),
           lam = 0.5, n = 20)
}
