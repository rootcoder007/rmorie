% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.profile_nll}
\alias{.abcgp.profile_nll}
\title{.abcgp.profile_nll}
\usage{
.abcgp.profile_nll(X, y, ls, nug, kernel)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{ls}{Passed to \code{.abcgp.corr}.}
\item{nug}{A vector; indexed elementwise.}
\item{kernel}{Passed to \code{.abcgp.corr}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.mle_lengthscale}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
