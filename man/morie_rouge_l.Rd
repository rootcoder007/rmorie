% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rouge_l}
\alias{morie_rouge_l}
\title{morie_rouge_l}
\usage{
morie_rouge_l(candidate, reference, beta = 1)
}
\arguments{
\item{candidate}{See Usage.}
\item{reference}{See Usage.}
\item{beta}{Defaults to \code{1}.}
}
\description{
Part of the rouge_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rouge_l(V, V)
}
