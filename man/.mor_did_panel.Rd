% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_did_panel}
\alias{.mor_did_panel}
\title{Balanced panel matrix from long-format columns.  Units and periods}
\usage{
.mor_did_panel(y, unit, time)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{unit}{Coerced to vector by the body, with \code{as.vector}.}
\item{time}{Coerced to vector by the body, with \code{as.vector}.}
}
\description{
are taken in sorted order, matching numpy\\'s unique().
}
\value{
A list with \code{Y}, \code{units}, \code{periods}.
}
\keyword{internal}
