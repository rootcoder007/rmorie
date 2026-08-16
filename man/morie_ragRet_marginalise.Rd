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
\item{doc.scores}{See Usage.}
\item{token.probs}{See Usage.}
\item{mode}{Defaults to \code{"sequence"}.}
}
\description{
Part of the ragRet_native implementation; see the file header for the source it follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_ragRet_marginalise(V, V)
}
