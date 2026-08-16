% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_rng}
\alias{.karpv_rng}
\title{.karpv_rng}
\usage{
.karpv_rng(seed)
}
\arguments{
\item{seed}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the karpV_native implementation. Called by \code{morie_karpV}. See the file header for the source the module follows.
}
\value{
The value of \code{e}, as built in the body.
}
\keyword{internal}
\examples{
.karpv_rng(seed = 5L)
}
