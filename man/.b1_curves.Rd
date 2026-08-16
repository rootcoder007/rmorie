% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b1_curves}
\alias{.b1_curves}
\title{.b1_curves}
\usage{
.b1_curves(Y, A, fit)
}
\arguments{
\item{Y}{A vector; its length is taken.}
\item{A}{Numeric; combined arithmetically in the body.}
\item{fit}{A list; the body reads \code{$g0}, \code{$g1}, \code{$Q0star}, \code{$Q1star}, \code{$QAstar} from it.}
}
\description{
A step of the b1_tmle implementation. Called by \code{Comptml}, \code{Tmleboot}, \code{Tmleor} and 3 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{mu1}, \code{mu0}, \code{ic1}, \code{ic0}.
}
\keyword{internal}
