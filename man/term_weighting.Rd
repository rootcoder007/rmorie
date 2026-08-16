% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{term_weighting}
\alias{term_weighting}
\title{term_weighting}
\usage{
term_weighting(X, how = "log_entropy")
}
\arguments{
\item{X}{See Usage.}
\item{how}{One of \code{"raw"}, \code{"tfidf"}. Defaults to \code{"log_entropy"}.}
}
\description{
A step of the lsa_native implementation. Called by \code{lsa_decompose}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
term_weighting(M)
}
