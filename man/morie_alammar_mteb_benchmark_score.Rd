% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_mteb_benchmark_score}
\alias{morie_alammar_mteb_benchmark_score}
\title{morie_alammar_mteb_benchmark_score}
\usage{
morie_alammar_mteb_benchmark_score(task_scores, category_map)
}
\arguments{
\item{task_scores}{Named list/vector.}
\item{category_map}{Named map   task -> category.}
}
\description{
morie_alammar_mteb_benchmark_score
}
\value{
A list with \code{estimate}, \code{category_means}, \code{flat_task_mean}, \code{weighting_matters}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_mteb_benchmark_score(task_scores = list(a = 1, b = 2), category_map = list(a = 1, b = 2))
}
