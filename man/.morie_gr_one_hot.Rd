% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_one_hot}
\alias{.morie_gr_one_hot}
\title{.morie_gr_one_hot}
\usage{
.morie_gr_one_hot(Y, K, m)
}
\arguments{
\item{Y}{A matrix; passed to \code{nrow}.}
\item{K}{A count; the body uses it as \code{matrix(...)}.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_gradient_matrix}, \code{morie_geron_softmax_cross_entropy_cost}. See the file header for the source the module follows.
}
\value{
The value of \code{Y}, as built in the body.
}
\keyword{internal}
\examples{
.morie_gr_one_hot(Y = c(0L, 2L, 1L), K = 3L, m = 3L)
}
