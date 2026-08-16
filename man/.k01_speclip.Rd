% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.k01_speclip}
\alias{.k01_speclip}
\title{.k01_speclip}
\usage{
.k01_speclip(Xm, p, iters = 50L)
}
\arguments{
\item{Xm}{A matrix; passed to \code{t}.}
\item{p}{A count; the body uses it as \code{rep(...)}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
}
\description{
A step of the acclso implementation. Called by \code{Agdproj}, \code{Fistalasso}. See the file header for the source the module follows.
}
\value{
The value of \code{lam}, as built in the body.
}
\keyword{internal}
