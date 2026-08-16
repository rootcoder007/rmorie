% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_shr}
\alias{.karpv_shr}
\title{.karpv_shr}
\usage{
.karpv_shr(a, k)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the karpV_native implementation. Called by \code{.karpv_u32}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.karpv_shr(V, V)
}
