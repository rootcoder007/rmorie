% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dbmirror}
\alias{.dbmirror}
\title{.dbmirror}
\usage{
.dbmirror(h)
}
\arguments{
\item{h}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the wave implementation. Called by \code{Wave}, \code{Wvltdb}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dbmirror(V)
}
