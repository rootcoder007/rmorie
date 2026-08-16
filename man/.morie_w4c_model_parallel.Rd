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
\item{sizes}{See Usage.}
\item{n_stages}{See Usage.}
}
\description{
minimising the maximum device load (prefix-sum greedy assignment).
}
\value{
A list with \code{assignment}, \code{device_loads}, \code{max_load}, \code{imbalance}.
}
\keyword{internal}
