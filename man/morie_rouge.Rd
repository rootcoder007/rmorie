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
\item{candidate}{Passed to \code{morie_rouge_l}.}
\item{reference}{Passed to \code{morie_rouge_l}.}
\item{variant}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"L"}.}
\item{n}{Passed to \code{morie_rouge_n}. Defaults to \code{1}.}
\item{alpha}{Passed to \code{morie_rouge_w}. Defaults to \code{1.2}.}
\item{beta}{Passed to \code{morie_rouge_l}. Defaults to \code{1}.}
}
\description{
A step of the rouge_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rouge(V, V)
}
