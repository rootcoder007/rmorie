% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.attrInf_sort_keys}
\alias{.attrInf_sort_keys}
\title{.attrInf_sort_keys}
\usage{
.attrInf_sort_keys(vals)
}
\arguments{
\item{vals}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the attrInf_native implementation. Called by \code{morie_attrInf}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.attrInf_sort_keys(V)
}
