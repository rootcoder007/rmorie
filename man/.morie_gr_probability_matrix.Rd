% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_probability_matrix}
\alias{.morie_gr_probability_matrix}
\title{.morie_gr_probability_matrix}
\usage{
.morie_gr_probability_matrix(X, theta)
}
\arguments{
\item{X}{Passed to \code{.morie_gr_score_matrix}.}
\item{theta}{Passed to \code{.morie_gr_score_matrix}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_gradient_matrix}, \code{morie_geron_softmax_cross_entropy_cost}, \code{morie_geron_softmax_probability}. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{theta}, \code{probabilities}, \code{scores}.
}
\keyword{internal}
