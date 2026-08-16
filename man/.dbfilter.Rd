% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dbfilter}
\alias{.dbfilter}
\title{Scaling filter from the exact radical closed form}
\usage{
.dbfilter(name)
}
\arguments{
\item{name}{Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the wave implementation. Called by \code{Wave}, \code{Wvltdb}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dbfilter(V)
}
