% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_cross_k}
\alias{.schab_cross_k}
\title{.schab_cross_k}
\usage{
.schab_cross_k(p1, p2, region, r, correction = "ripley")
}
\arguments{
\item{p1}{A matrix; indexed by row and column.}
\item{p2}{A matrix; indexed by row and column.}
\item{region}{A vector; indexed elementwise.}
\item{r}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{correction}{One of \code{"none"}, \code{"ripley"}. Defaults to \code{"ripley"}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_cross_k_combined}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
