% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpclassmetrics}
\alias{.gpclassmetrics}
\title{.gpclassmetrics}
\usage{
.gpclassmetrics(conf, i)
}
\arguments{
\item{conf}{A matrix; indexed by row and column.}
\item{i}{See Usage.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm007}, \code{Msm008}. See the file header for the source the module follows.
}
\value{
A list with \code{TFN}, \code{TFP}, \code{TTN}, \code{TTP_all}, \code{precision}, \code{sensitivity}, \code{specificity}, \code{pCCC}.
}
\keyword{internal}
