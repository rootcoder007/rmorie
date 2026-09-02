% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{draw_projections}
\alias{draw_projections}
\title{draw_projections}
\usage{
draw_projections(m, d, seed = 0L, orthogonal = TRUE) 
}
\arguments{
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
\item{d}{A count; the body uses it as \code{matrix(...)}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
\item{orthogonal}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the perfat_native implementation. Called by \code{favor_attention}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
draw_projections(m = 5L, d = 5L)
}
