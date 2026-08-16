% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_k_border}
\alias{.schab_k_border}
\title{Single-pattern K with the border ("reduced sample") correction, as in}
\usage{
.schab_k_border(p, region, r)
}
\arguments{
\item{p}{A matrix; indexed by row and column.}
\item{region}{A vector; indexed elementwise.}
\item{r}{Iterated over elementwise, with \code{vapply}.}
}
\description{
the Python arm\\'s k_function; needed by Diggle-Chetwynd\\'s D(h).
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
