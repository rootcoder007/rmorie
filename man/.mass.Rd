% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mass}
\alias{.mass}
\title{.mass}
\usage{
.mass(ts, ys, lo, hi)
}
\arguments{
\item{ts}{A vector; its length is taken and its elements indexed.}
\item{ys}{A vector; indexed elementwise.}
\item{lo}{Numeric; passed to \code{max}.}
\item{hi}{Numeric; passed to \code{min}.}
}
\description{
A step of the qrntcq_native implementation. Called by \code{morie_qrntcq_quarantine_efficacy}. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
