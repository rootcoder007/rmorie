% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TwidConj}
\alias{TwidConj}
\title{Eq (3.88): W_N^(-nk) = conj(W_N^(nk)) -- a negative power costs only}
\usage{
TwidConj(npoints, n, k)
}
\arguments{
\item{npoints}{See Usage.}
\item{n}{See Usage.}
\item{k}{See Usage.}
}
\description{
a sign flip, one of the two properties the FFT is built on.
}
\value{
A list with \code{negative_power}, \code{conjugate}, \code{difference}, \code{holds}, \code{N}, \code{n}, \code{k}, \code{method}.
}
\keyword{internal}
