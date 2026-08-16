% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02p2t}
\alias{k02p2t}
\title{k02p2t}
\usage{
k02p2t(tv, df)
}
\arguments{
\item{tv}{Numeric; passed to \code{abs}.}
\item{df}{See Usage.}
}
\description{
A step of the k02util implementation. Called by \code{mahks}, \code{mahsj}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02p2t(V, V)
}
