% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsalpc}
\alias{.bsalpc}
\title{Levinson-Durbin linear prediction; returns list(a = a[1..p], e =}
\usage{
.bsalpc(x, order)
}
\arguments{
\item{x}{Passed to \code{.bsaacf}.}
\item{order}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
error) Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{a}, \code{e}.
}
\keyword{internal}
\examples{
.bsalpc(x = c(1, 2, 3, 4, 5, 6, 7, 8), order = 5L)
}
