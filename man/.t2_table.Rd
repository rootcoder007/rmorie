% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t2_table}
\alias{.t2_table}
\title{.t2_table}
\usage{
.t2_table(X)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the tail2_t02 implementation. Called by \code{CttAlpha}, \code{CttAlphaMax}. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t2_table(V)
}
