% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.predict_tree}
\alias{.predict_tree}
\title{Recursive descent through one tree to the leaf containing x}
\usage{
.predict_tree(tree, x)
}
\arguments{
\item{tree}{A list; the body reads \code{$cut}, \code{$j}, \code{$leaf}, \code{$left}, \code{$right}, \code{$value} from it.}
\item{x}{A vector; indexed elementwise.}
}
\description{
A step of the orfgrf_native implementation. Called by \code{.orfgrf_forest_weights}. See the file header for the source the module follows.
}
\value{
The value of \code{.predict_tree}.
}
\keyword{internal}
