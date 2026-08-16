% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cf_walk}
\alias{.morie_cf_walk}
\title{.morie_cf_walk}
\usage{
.morie_cf_walk(node, xrow)
}
\arguments{
\item{node}{A list; the body reads \code{$feature}, \code{$left}, \code{$right}, \code{$tau}, \code{$threshold} from it.}
\item{xrow}{A vector; indexed elementwise.}
}
\description{
A step of the causal_forest_honest implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
