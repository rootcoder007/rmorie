% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr2_onehot}
\alias{.morie_gr2_onehot}
\title{.morie_gr2_onehot}
\usage{
.morie_gr2_onehot(Y, m, K)
}
\arguments{
\item{Y}{See Usage.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
\item{K}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
A step of the geron_ml2_native implementation. Called by \code{morie_geron_cross_entropy_cost}, \code{morie_geron_cross_entropy_gradient}. See the file header for the source the module follows.
}
\value{
The value of \code{Yoh}, as built in the body.
}
\keyword{internal}
