% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.log_evidence}
\alias{.log_evidence}
\title{.log_evidence}
\usage{
.log_evidence(D, Tn, Vn, z, mm, nn, a, b)
}
\arguments{
\item{D}{A vector; its length is taken.}
\item{Tn}{Passed to \code{.counts}.}
\item{Vn}{Passed to \code{.counts}.}
\item{z}{Passed to \code{.counts}.}
\item{mm}{A vector; indexed elementwise.}
\item{nn}{A vector; indexed elementwise.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bigtm_native implementation. Called by \code{log_evidence}. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
