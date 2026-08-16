% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr2_best_split}
\alias{.morie_gr2_best_split}
\title{.morie_gr2_best_split}
\usage{
.morie_gr2_best_split(X, y, criterion, min_samples_leaf)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{Passed to \code{morie_geron_cart_split_cost}.}
\item{criterion}{Passed to \code{morie_geron_cart_split_cost}.}
\item{min_samples_leaf}{See Usage.}
}
\description{
A step of the geron_ml2_native implementation. Called by \code{.morie_gr2_grow}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
