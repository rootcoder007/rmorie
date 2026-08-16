% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_slogdet}
\alias{.gwasem_slogdet}
\title{.gwasem_slogdet}
\usage{
.gwasem_slogdet(M)
}
\arguments{
\item{M}{See Usage.}
}
\description{
A step of the gwasem_native implementation. Called by \code{.gwasem_loglik}, \code{morie_gwasem_reml}. See the file header for the source the module follows.
}
\value{
A list with \code{sign}, \code{logdet}.
}
\keyword{internal}
