% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_score_matrix}
\alias{.morie_gr_score_matrix}
\title{.morie_gr_score_matrix}
\usage{
.morie_gr_score_matrix(X, theta)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{theta}{See Usage.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_probability_matrix}, \code{morie_geron_softmax_score_grsmxs}. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{theta}, \code{scores}.
}
\keyword{internal}
