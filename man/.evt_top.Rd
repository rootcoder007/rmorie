% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.evt_top}
\alias{.evt_top}
\title{.evt_top}
\usage{
.evt_top(x, k)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the evt_native implementation. Called by \code{morie_evt_dedh}, \code{morie_evt_hill}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
.evt_top(x = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
