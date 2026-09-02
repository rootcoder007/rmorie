% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{pool}
\alias{pool}
\title{pool}
\usage{
pool(token_vectors, mode = "mean", mask = NULL) 
}
\arguments{
\item{token_vectors}{Passed to \code{.sbert_mat}.}
\item{mode}{One of \code{"cls"}, \code{"max"}. Defaults to \code{"mean"}.}
\item{mask}{The body requires: sbert: the mask excludes every token.}
}
\description{
A step of the sbert_native implementation. Called by \code{morie_geron_lenet5}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
pool(V)
}
