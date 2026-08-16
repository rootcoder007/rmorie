% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_bnd_one_mean}
\alias{.mor_bnd_one_mean}
\title{.mor_bnd_one_mean}
\usage{
.mor_bnd_one_mean(yv, seen, k0, k1)
}
\arguments{
\item{yv}{A vector; indexed elementwise.}
\item{seen}{Numeric; passed to \code{mean}.}
\item{k0}{Numeric; combined arithmetically in the body.}
\item{k1}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bndest_native implementation. Called by \code{morie_bndest}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
