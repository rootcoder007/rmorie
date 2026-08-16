% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ragRet_recall_at_k}
\alias{morie_ragRet_recall_at_k}
\title{morie_ragRet_recall_at_k}
\usage{
morie_ragRet_recall_at_k(approximate, exact)
}
\arguments{
\item{approximate}{See Usage.}
\item{exact}{See Usage.}
}
\description{
A step of the ragRet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{recall}, \code{hits}, \code{k}, \code{missed}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_ragRet_recall_at_k(V, V)
}
