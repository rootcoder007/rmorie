% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{preference_vector}
\alias{preference_vector}
\title{preference_vector}
\usage{
preference_vector(nodes, focus, weight = 0.9)
}
\arguments{
\item{nodes}{Coerced to list by the body, with \code{as.list}.}
\item{focus}{See Usage.}
\item{weight}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.9}.}
}
\description{
A step of the tagRC_native implementation. Called by \code{folkrank}. See the file header for the source the module follows.
}
\value{
A list with \code{p}, \code{focus}, \code{mass}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
preference_vector(V, V)
}
