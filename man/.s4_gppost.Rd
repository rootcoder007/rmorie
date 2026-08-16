% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_gppost}
\alias{.s4_gppost}
\title{.s4_gppost}
\usage{
.s4_gppost(K, Ks, Kss, y, noise = 1e-06)
}
\arguments{
\item{K}{A matrix; passed to \code{nrow}.}
\item{Ks}{A matrix; passed to \code{crossprod}.}
\item{Kss}{See Usage.}
\item{y}{See Usage.}
\item{noise}{A matrix; passed to \code{diag}. Defaults to \code{1e-06}.}
}
\description{
A step of the s04core implementation. Called by \code{Gpmoe}. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{var}.
}
\keyword{internal}
