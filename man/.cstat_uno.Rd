% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cstat_uno}
\alias{.cstat_uno}
\title{.cstat_uno}
\usage{
.cstat_uno(time, event, risk_score)
}
\arguments{
\item{time}{A vector; its length is taken and its elements indexed.}
\item{event}{A vector; indexed elementwise.}
\item{risk_score}{A vector; indexed elementwise.}
}
\description{
A step of the Cstat implementation. Called by \code{Cstat}. See the file header for the source the module follows.
}
\value{
A list with \code{c_statistic}, \code{se}, \code{ci_lower}, \code{ci_upper}, \code{concordant}, \code{discordant}, \code{tied}, \code{comparable}.
}
\keyword{internal}
