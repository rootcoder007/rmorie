% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_model_parallel}
\alias{.morie_w4c_model_parallel}
\title{Contiguous greedy partition of layer sizes into n_stages devices,}
\usage{
.morie_w4c_model_parallel(sizes, n_stages)
}
\arguments{
\item{sizes}{A vector; its length is taken and its elements indexed.}
\item{n_stages}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
minimising the maximum device load (prefix-sum greedy assignment).
}
\value{
A list with \code{assignment}, \code{device_loads}, \code{max_load}, \code{imbalance}.
}
\keyword{internal}
\examples{
.morie_w4c_model_parallel(sizes = c(1, 2, 3, 4, 5, 6, 7, 8), n_stages = 5L)
}
