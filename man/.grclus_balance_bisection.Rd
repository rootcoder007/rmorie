% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_balance_bisection}
\alias{.grclus_balance_bisection}
\title{.grclus_balance_bisection}
\usage{
.grclus_balance_bisection(adj, vw, parts, target, tolerance)
}
\arguments{
\item{adj}{A vector; its length is taken.}
\item{vw}{A vector; indexed elementwise.}
\item{parts}{A vector; indexed elementwise.}
\item{target}{Numeric; combined arithmetically in the body.}
\item{tolerance}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_bisect}. See the file header for the source the module follows.
}
\value{
The value of \code{parts}, as built in the body.
}
\keyword{internal}
