% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.orfgrf_forest_weights}
\alias{.orfgrf_forest_weights}
\title{Forest kernel weights alpha_i(x) for a single evaluation point}
\usage{
.orfgrf_forest_weights(trees, X, x)
}
\arguments{
\item{trees}{See Usage.}
\item{X}{See Usage.}
\item{x}{See Usage.}
}
\description{
Each unit that lands in the same leaf as x in tree t contributes 1 / (#units from tree t\\\'s leaf-sample in that leaf).
}
\value{
A numeric value.
}
\keyword{internal}
