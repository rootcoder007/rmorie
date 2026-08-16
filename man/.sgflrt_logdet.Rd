% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_logdet}
\alias{.sgflrt_logdet}
\title{.sgflrt_logdet}
\usage{
.sgflrt_logdet(L)
}
\arguments{
\item{L}{A matrix; passed to \code{diag}.}
}
\description{
A step of the sgflrt_native implementation. Called by \code{.sgflrt_laplace}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
