% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vitlnrows}
\alias{.vitlnrows}
\title{.vitlnrows}
\usage{
.vitlnrows(A, eps = .vitlneps)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{eps}{Passed to \code{.vitln}. Defaults to \code{.vitlneps}.}
}
\description{
A step of the helpers_vit implementation. Called by \code{Vitfwd}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
