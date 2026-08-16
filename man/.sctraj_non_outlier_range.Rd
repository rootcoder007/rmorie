% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_non_outlier_range}
\alias{.sctraj_non_outlier_range}
\title{Lowest and highest non-outlier values, by the 1.5 IQR rule}
\usage{
.sctraj_non_outlier_range(vals)
}
\arguments{
\item{vals}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sctraj_non_outlier_range(V)
}
