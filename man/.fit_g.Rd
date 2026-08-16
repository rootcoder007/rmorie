% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fit_g}
\alias{.fit_g}
\title{.fit_g}
\usage{
.fit_g(A, W, aL, aH, rows, ridge, trim)
}
\arguments{
\item{A}{A vector; its length is taken.}
\item{W}{A matrix; passed to \code{ncol}.}
\item{aL}{See Usage.}
\item{aH}{See Usage.}
\item{rows}{See Usage.}
\item{ridge}{Accepted by the signature and not used anywhere in the body.}
\item{trim}{Numeric; passed to \code{max}.}
}
\description{
A step of the tmldta_native implementation. Called by \code{split_specific_tmle}. See the file header for the source the module follows.
}
\value{
A list with \code{gH}, \code{gL}.
}
\keyword{internal}
