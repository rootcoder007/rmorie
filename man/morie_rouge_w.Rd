% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rouge_w}
\alias{morie_rouge_w}
\title{morie_rouge_w}
\usage{
morie_rouge_w(candidate, reference, alpha = 1.2, beta = 1) 
}
\arguments{
\item{candidate}{Passed to \code{.rouge_toks}.}
\item{reference}{Passed to \code{.rouge_get_refs_simple}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{1.2}.}
\item{beta}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the rouge_native implementation. Called by \code{morie_rouge}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rouge_w(V, V)
}
