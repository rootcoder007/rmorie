% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ragRet_marginalise}
\alias{morie_ragRet_marginalise}
\title{morie_ragRet_marginalise}
\usage{
morie_ragRet_marginalise(doc.scores, token.probs, mode = "sequence")
}
\arguments{
\item{doc.scores}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{token.probs}{Iterated over elementwise, with \code{lapply}.}
\item{mode}{One of \code{"sequence"}, \code{"token"}. Defaults to \code{"sequence"}.}
}
\description{
A step of the ragRet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_ragRet_marginalise(V, V)
}
