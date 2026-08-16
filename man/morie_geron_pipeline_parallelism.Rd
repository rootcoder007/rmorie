% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_pipeline_parallelism}
\alias{morie_geron_pipeline_parallelism}
\title{Pipeline parallelism: contiguous stage partition plus a microbatch schedule (Geron Ch 17, hmppp)}
\usage{
morie_geron_pipeline_parallelism(model, n_stages, n_microbatches = 4)
}
\arguments{
\item{model}{Sequence of layer sizes (or weight arrays; lengths are used).}
\item{n_stages}{Pipeline stages.}
\item{n_microbatches}{Microbatches per batch.}
}
\description{
Pipeline parallelism: contiguous stage partition plus a microbatch schedule (Geron Ch 17, hmppp)
}
\value{
A list with \code{assignment}, \code{stage_loads}, \code{max_load}, \code{imbalance}, \code{bubble_fraction}, \code{utilisation}, \code{schedule}, \code{n_slots}, \code{n_microbatches}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_geron_pipeline_parallelism(model = c(1, 2, 3, 4, 5, 6, 7, 8), n_stages = 5L)
}
