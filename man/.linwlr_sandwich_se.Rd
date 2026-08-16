% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.linwlr_sandwich_se}
\alias{.linwlr_sandwich_se}
\title{.linwlr_sandwich_se}
\usage{
.linwlr_sandwich_se(bread, meat, n, ridge)
}
\arguments{
\item{bread}{A matrix; passed to \code{nrow}.}
\item{meat}{A matrix; passed to \code{\%*\%}.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the linwlr_native implementation. Called by \code{morie_linwlr}. See the file header for the source the module follows.
}
\value{
The value of \code{se}, as built in the body.
}
\keyword{internal}
