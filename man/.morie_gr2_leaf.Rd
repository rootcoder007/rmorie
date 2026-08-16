% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr2_leaf}
\alias{.morie_gr2_leaf}
\title{.morie_gr2_leaf}
\usage{
.morie_gr2_leaf(y, criterion)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{criterion}{One of \code{"gini"}, \code{"mse"}.}
}
\description{
A step of the geron_ml2_native implementation. Called by \code{.morie_gr2_grow}. See the file header for the source the module follows.
}
\value{
A list with \code{leaf}, \code{value}, \code{proba}, \code{n}, \code{impurity}.
}
\keyword{internal}
