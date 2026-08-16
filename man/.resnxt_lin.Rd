% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.resnxt_lin}
\alias{.resnxt_lin}
\title{.resnxt_lin}
\usage{
.resnxt_lin(W, x)
}
\arguments{
\item{W}{A vector; its length is taken and its elements indexed.}
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the resnxt_native implementation. Called by \code{.resnxt_aggregated_block}, \code{.resnxt_grouped_block}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.resnxt_lin(V, V)
}
