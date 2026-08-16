% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_average_curve}
\alias{morie_sctraj_average_curve}
\title{morie_sctraj_average_curve}
\usage{
morie_sctraj_average_curve(curves, n_points = 100, return_grid = FALSE)
}
\arguments{
\item{curves}{A vector; its length is taken and its elements indexed.}
\item{n_points}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.}
\item{return_grid}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
