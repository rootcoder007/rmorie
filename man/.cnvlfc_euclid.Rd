% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cnvlfc_euclid}
\alias{.cnvlfc_euclid}
\title{.cnvlfc_euclid}
\usage{
.cnvlfc_euclid(target, candidates, exclude_idx, target_idx, exclude_window)
}
\arguments{
\item{target}{A matrix; indexed by row and column.}
\item{candidates}{A matrix; indexed by row and column.}
\item{exclude_idx}{A vector; indexed elementwise.}
\item{target_idx}{Numeric; combined arithmetically in the body.}
\item{exclude_window}{Passed to \code{<=}.}
}
\description{
A step of the cnvlfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{d}, \code{ord}, \code{keep}.
}
\keyword{internal}
