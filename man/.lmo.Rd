% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lmo}
\alias{.lmo}
\title{Linear minimisation oracle}
\usage{
.lmo(gradient, domain, lower, upper)
}
\arguments{
\item{gradient}{A vector; its length is taken and its elements indexed.}
\item{domain}{Compared against \code{"simplex"}.}
\item{lower}{A vector; indexed elementwise.}
\item{upper}{A vector; indexed elementwise.}
}
\description{
A step of the qpdual_native implementation. Called by \code{morie_qpdual}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
