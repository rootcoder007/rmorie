% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.resnxt_block_parameters}
\alias{.resnxt_block_parameters}
\title{.resnxt_block_parameters}
\usage{
.resnxt_block_parameters(width, cardinality, bottleneck)
}
\arguments{
\item{width}{See Usage.}
\item{cardinality}{See Usage.}
\item{bottleneck}{See Usage.}
}
\description{
A step of the resnxt_native implementation. Called by \code{.resnxt_match_complexity}. See the file header for the source the module follows.
}
\value{
A list with \code{parameters}, \code{cardinality}, \code{bottleneck}, \code{width}.
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
