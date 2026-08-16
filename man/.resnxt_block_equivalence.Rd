% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.resnxt_block_equivalence}
\alias{.resnxt_block_equivalence}
\title{.resnxt_block_equivalence}
\usage{
.resnxt_block_equivalence(x, Wins, Wmids, Wouts, tol = 1e-09)
}
\arguments{
\item{x}{Passed to \code{.resnxt_aggregated_block}.}
\item{Wins}{Passed to \code{.resnxt_aggregated_block}.}
\item{Wmids}{Passed to \code{.resnxt_aggregated_block}.}
\item{Wouts}{A vector; indexed elementwise.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
}
\description{
A step of the resnxt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{equivalent}, \code{max_deviation}, \code{aggregated}, \code{grouped}, \code{note}.
}
\references{
Xie, S., Girshick, R., Dollar, P., Tu, Z. & He, K. (2017) "Aggregated
Residual Transformations for Deep Neural Networks", CVPR 2017, 5987-5995,
arXiv:1611.05431.
He, K., Zhang, X., Ren, S. & Sun, J. (2016) "Deep Residual Learning for
Image Recognition", CVPR 2016, 770-778, arXiv:1512.03385.
Szegedy, C. et al. (2015) "Going deeper with convolutions", CVPR 2015,
1-9.
}
\keyword{internal}
