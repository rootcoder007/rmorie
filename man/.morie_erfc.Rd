% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_erfc}
\alias{.morie_erfc}
\title{Exact identity erfc(v) = Q(1/2, v^2): no transcribed continued}
\usage{
.morie_erfc(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
fraction of its own, and the tail comes straight from the CF branch of the incomplete gamma, which computes Q without forming 1 - P.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
