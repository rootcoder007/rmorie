% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.Fun}
\alias{.Fun}
\title{An R closure carrying whatever derivatives were supplied. Mirrors}
\usage{
.Fun(f, grad = NULL, hess = NULL, affine = FALSE)
}
\arguments{
\item{f}{Carried through into a list the body builds.}
\item{grad}{Carried through into a list the body builds.}
\item{hess}{Carried through into a list the body builds.}
\item{affine}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
the Python _Fun class field-for-field.
}
\value{
The value of \code{self}, as built in the body.
}
\keyword{internal}
