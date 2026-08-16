% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{map_invert}
\alias{map_invert}
\title{map_invert}
\usage{
map_invert(model, y, known, candidates, err, priors, sensitive = 0L)
}
\arguments{
\item{model}{See Usage.}
\item{y}{See Usage.}
\item{known}{See Usage.}
\item{candidates}{A vector; its length is taken.}
\item{err}{See Usage.}
\item{priors}{A vector; indexed elementwise.}
\item{sensitive}{Defaults to \code{0L}.}
}
\description{
A step of the attrInf_native implementation. Called by \code{morie_attrInf}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{scores}.
}
\keyword{internal}
