% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.resnxt_aggregated_block}
\alias{.resnxt_aggregated_block}
\title{.resnxt_aggregated_block}
\usage{
.resnxt_aggregated_block(x, Wins, Wmids, Wouts)
}
\arguments{
\item{x}{Passed to \code{.resnxt_vec}.}
\item{Wins}{A vector; its length is taken and its elements indexed.}
\item{Wmids}{A vector; indexed elementwise.}
\item{Wouts}{A vector; indexed elementwise.}
}
\description{
A step of the resnxt_native implementation. Called by \code{.resnxt_block_equivalence}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
.resnxt_aggregated_block(x = c(1, 2, 3, 4, 5, 6, 7, 8), Wins = c(1, 2, 3, 4, 5, 6, 7, 8), Wmids = c(1, 2, 3, 4, 5, 6, 7, 8), Wouts = c(1, 2, 3, 4, 5, 6, 7, 8))
}
