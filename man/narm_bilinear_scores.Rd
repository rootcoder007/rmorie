% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_bilinear_scores}
\alias{narm_bilinear_scores}
\title{narm_bilinear_scores}
\usage{
narm_bilinear_scores(embeddings, B, c_t)
}
\arguments{
\item{embeddings}{A matrix; passed to \code{as.matrix}.}
\item{B}{A matrix; passed to \code{ncol}.}
\item{c_t}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the narm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{scores}, \code{probabilities}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
narm_bilinear_scores(embeddings = c(1, 2, 3, 4, 5, 6, 7, 8), B = 5L, c_t = 5L)
}
