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
\item{trees}{A list; the body reads \code{$trees} from it.}
\item{X}{A matrix; indexed by row and column.}
\item{x}{A vector; indexed elementwise.}
}
\description{
Each unit that lands in the same leaf as x in tree t contributes 1 / (#units from tree t\\'s leaf-sample in that leaf).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.orfgrf_forest_weights(trees = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
