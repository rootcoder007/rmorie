% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_directions}
\alias{.ot_directions}
\title{.ot_directions}
\usage{
.ot_directions(d, n_proj)
}
\arguments{
\item{d}{A count; the body uses it as \code{matrix(...)}.}
\item{n_proj}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{Otmsw}, \code{Otsd}, \code{Otsw}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.ot_directions(d = 5L, n_proj = 5L)
}
