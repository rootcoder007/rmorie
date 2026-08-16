% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_loglik}
\alias{.gwasem_loglik}
\title{.gwasem_loglik}
\usage{
.gwasem_loglik(yt, Xt, d, ml)
}
\arguments{
\item{yt}{A vector; its length is taken.}
\item{Xt}{A matrix; indexed by row and column.}
\item{d}{Numeric; passed to \code{log}.}
\item{ml}{A flag; the body branches on it.}
}
\description{
A step of the gwasem_native implementation. Called by \code{.gwasem_reml_delta}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
