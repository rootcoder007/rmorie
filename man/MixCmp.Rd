% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MixCmp}
\alias{MixCmp}
\title{Section 9.7.4.  The three decompositions answer different questions}
\usage{
MixCmp(X, ncomp = NULL, maxiter = 200, seed = 1)
}
\arguments{
\item{X}{Passed to \code{.morie_bx_mat}.}
\item{ncomp}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{maxiter}{Defaults to \code{200}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
of the same data -- uncorrelated, independent, nonnegative-parts -- and which to use is empirical, so reconstruct with each and measure.  NMF runs on the nonnegatively shifted mixture, as eq (9.46) requires.
}
\value{
A list with \code{error}, \code{best}, \code{components}, \code{rank}, \code{method}.
}
\keyword{internal}
