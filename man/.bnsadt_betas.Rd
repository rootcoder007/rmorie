% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnsadt_betas}
\alias{.bnsadt_betas}
\title{Per-interval publication probabilities from the free parameters}
\usage{
.bnsadt_betas(family, params)
}
\arguments{
\item{family}{See Usage.}
\item{params}{A vector; indexed elementwise.}
}
\description{
A step of the bnsadt_native implementation. Called by \code{.bnsadt_expected_p}, \code{.bnsadt_pub_cdf}, \code{morie_bnsadt_p}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
