% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_gradient_matrix}
\alias{.morie_gr_gradient_matrix}
\title{.morie_gr_gradient_matrix}
\usage{
.morie_gr_gradient_matrix(X, Y, theta)
}
\arguments{
\item{X}{Passed to \code{.morie_gr_probability_matrix}.}
\item{Y}{Passed to \code{.morie_gr_one_hot}.}
\item{theta}{Passed to \code{.morie_gr_probability_matrix}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_ch4_cross_entropy_gradient_vector}, \code{morie_geron_softmax_cost_gradient}. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{P}, \code{Yh}, \code{G}.
}
\keyword{internal}
\examples{
set.seed(7)
X <- cbind(1, rnorm(20))
Y <- diag(3)[sample(1:3, 20, replace = TRUE), ]
theta <- matrix(0, 2, 3)
.morie_gr_gradient_matrix(X, Y, theta)
}
