% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rouge}
\alias{morie_rouge}
\title{morie_rouge}
\usage{
morie_rouge(candidate, reference, variant = "L", n = 1, alpha = 1.2, beta = 1)
}
\arguments{
\item{candidate}{See Usage.}
\item{reference}{See Usage.}
\item{variant}{Defaults to \code{"L"}.}
\item{n}{Defaults to \code{1}.}
\item{alpha}{Defaults to \code{1.2}.}
\item{beta}{Defaults to \code{1}.}
}
\description{
Part of the rouge_native implementation; see the file header for the source it follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rouge(V, V)
}
