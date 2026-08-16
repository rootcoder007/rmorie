% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_project}
\alias{.sctraj_project}
\title{Nearest point on the polyline, and its arc length}
\usage{
.sctraj_project(point, curve, s)
}
\arguments{
\item{point}{Numeric; combined arithmetically in the body.}
\item{curve}{A matrix; indexed by row and column.}
\item{s}{A vector; indexed elementwise.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_principal_curve}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
