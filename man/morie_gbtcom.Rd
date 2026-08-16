% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gbtcom}
\alias{morie_gbtcom}
\title{Goodman-Bacon decomposition, vector interface}
\usage{
morie_gbtcom(y, D, unit, time)
}
\arguments{
\item{y}{Outcome, one entry per unit-period.}
\item{D}{Treatment indicator, same length.}
\item{unit}{Unit identifier, same length.}
\item{time}{Period identifier, same length.}
}
\description{
The signature-compatible mirror of Python \code{morie.fn.gbtcom}, which takes the four columns as vectors. \code{Gbtcom} keeps the data-frame form for R callers who already hold a panel.
}
\value{
As \code{Gbtcom}.
}
\keyword{internal}
