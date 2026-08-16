% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.resnxt_vec}
\alias{.resnxt_vec}
\title{.resnxt_vec}
\usage{
.resnxt_vec(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the resnxt_native implementation. Called by \code{.resnxt_aggregated_block}, \code{.resnxt_grouped_block}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
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
.resnxt_vec(V)
}
