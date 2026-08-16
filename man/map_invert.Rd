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
\item{model}{Accepted by the signature and not used anywhere in the body.}
\item{y}{Passed to \code{err}.}
\item{known}{See Usage.}
\item{candidates}{A vector; its length is taken.}
\item{err}{Accepted by the signature and not used anywhere in the body.}
\item{priors}{A vector; indexed elementwise.}
\item{sensitive}{Coerced to character by the body, with \code{as.character}. Defaults to \code{0L}.}
}
\description{
A step of the attrInf_native implementation. Called by \code{morie_attrInf}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{scores}.
}
\keyword{internal}
