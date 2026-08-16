% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.if_resolve}
\alias{.if_resolve}
\title{.if_resolve}
\usage{
.if_resolve(estimator, who)
}
\arguments{
\item{estimator}{One of \code{"mean"}, \code{"median"}, \code{"var"}.}
\item{who}{Passed to \code{paste0}.}
}
\description{
A step of the infcrv implementation. Called by \code{Btvinf}, \code{Infcrv}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
