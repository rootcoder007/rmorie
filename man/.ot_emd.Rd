% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_emd}
\alias{.ot_emd}
\title{.ot_emd}
\usage{
.ot_emd(a, b, C, max_pivots = 20000L)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
\item{C}{A matrix; passed to \code{nrow}.}
\item{max_pivots}{Defaults to \code{20000L}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{.ot_partial_plan}, \code{Otbarfree}, \code{Otemd} and 6 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{T}, \code{cost}.
}
\keyword{internal}
