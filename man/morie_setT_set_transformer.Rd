% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_setT_set_transformer}
\alias{morie_setT_set_transformer}
\title{Back-compatible wrapper over \code{setT}}
\usage{
morie_setT_set_transformer(X = NULL, k = NULL, S = NULL, params = NULL) 
}
\arguments{
\item{X}{Numeric matrix \code{n x d}.}
\item{k}{Ignored; \code{S} supplies the output count.}
\item{S}{Numeric matrix \code{k x d}.}
\item{params}{List of weights.}
}
\description{
Back-compatible wrapper over \code{setT}
}
\value{
As for \code{setT}.
}
\keyword{internal}
