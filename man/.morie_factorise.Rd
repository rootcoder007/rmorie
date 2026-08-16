% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_factorise}
\alias{.morie_factorise}
\title{Trial division stops once p^2 exceeds what is left, at which point}
\usage{
.morie_factorise(x, exps, primes)
}
\arguments{
\item{x}{See Usage.}
\item{exps}{See Usage.}
\item{primes}{See Usage.}
}
\description{
the remainder is itself prime -- it must still be RECORDED, not discarded. Dropping it silently would understate the hook product and make the division look inexact.
}
\value{
A list with \code{exps}, \code{remainder}.
}
\keyword{internal}
