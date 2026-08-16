% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_t2_fdgrad}
\alias{.morie_t2_fdgrad}
\title{.morie_t2_fdgrad}
\usage{
.morie_t2_fdgrad(fun, x, h)
}
\arguments{
\item{fun}{See Usage.}
\item{x}{A vector; its length is taken.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tail2_t03 implementation. Called by \code{PenaltyMin}. See the file header for the source the module follows.
}
\value{
The value of \code{g}, as built in the body.
}
\keyword{internal}
