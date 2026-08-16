% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_momento_task_mask}
\alias{morie_momento_task_mask}
\title{Construct a task-appropriate mask}
\usage{
morie_momento_task_mask(n_patches, task = "forecast", span = 1, start = NULL)
}
\arguments{
\item{n_patches}{Number of patches.}
\item{task}{One of forecast, impute, classify, anomaly.}
\item{span}{Span of the mask.}
\item{start}{Starting patch (for impute).}
}
\description{
Construct a task-appropriate mask
}
\value{
Integer vector of mask indices.
}
\keyword{internal}
\examples{
morie_momento_task_mask(n_patches = 5L)
}
