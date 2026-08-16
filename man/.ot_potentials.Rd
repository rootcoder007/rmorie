% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_potentials}
\alias{.ot_potentials}
\title{.ot_potentials}
\usage{
.ot_potentials(basis, C, n, m)
}
\arguments{
\item{basis}{Passed to \code{.ot_adj}.}
\item{C}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{numeric(...)}.}
\item{m}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{.ot_emd}. See the file header for the source the module follows.
}
\value{
A list with \code{u}, \code{v}.
}
\keyword{internal}
