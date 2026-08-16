% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03jacobi}
\alias{.s03jacobi}
\title{Symmetric eigenproblem by cyclic Jacobi.  Values ascending; each}
\usage{
.s03jacobi(A, sweeps = 60L)
}
\arguments{
\item{A}{See Usage.}
\item{sweeps}{Defaults to \code{60L}.}
}
\description{
vector sign-fixed so its largest-magnitude entry is positive, because the eigenproblem does not determine the sign and the arms must agree.
}
\value{
A list with \code{values}, \code{vectors}.
}
\keyword{internal}
