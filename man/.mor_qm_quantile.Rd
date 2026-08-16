% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_qm_quantile}
\alias{.mor_qm_quantile}
\title{Inverse empirical CDF, type-7 convention h = (n - 1) p}
\usage{
.mor_qm_quantile(sorted_x, p)
}
\arguments{
\item{sorted_x}{A vector; its length is taken and its elements indexed.}
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the qmDS_native implementation. Called by \code{morie_qmDS}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.mor_qm_quantile(sorted_x = c(1, 2, 3, 4, 5, 6, 7, 8), p = 0.5)
}
