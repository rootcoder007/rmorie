% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nashq_select}
\alias{.nashq_select}
\title{.nashq_select}
\usage{
.nashq_select(A, B, selection, agent, tol)
}
\arguments{
\item{A}{Numeric; passed to \code{max}.}
\item{B}{Numeric; passed to \code{max}.}
\item{selection}{One of \code{"best_for_agent"}, \code{"first"}, \code{"global_optimal"}.}
\item{agent}{See Usage.}
\item{tol}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the nashq_native implementation. Called by \code{morie_nashq}. See the file header for the source the module follows.
}
\value{
The value of \code{[[}.
}
\keyword{internal}
