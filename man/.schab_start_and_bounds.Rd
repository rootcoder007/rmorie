% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_start_and_bounds}
\alias{.schab_start_and_bounds}
\title{The bounds ARE the parameter space of Sec. 4.3: a nugget and a}
\usage{
.schab_start_and_bounds(lags, ghat)
}
\arguments{
\item{lags}{See Usage.}
\item{ghat}{See Usage.}
}
\description{
partial sill are variances so they are non-negative, and a range is a distance so it is strictly positive. Nothing narrower -- a fit that cannot reach sill = 0 cannot report "no spatial structure", and one that cannot reach nugget = 0 cannot report a continuous field.
}
\value{
A list with \code{start}, \code{lo}, \code{hi}.
}
\keyword{internal}
