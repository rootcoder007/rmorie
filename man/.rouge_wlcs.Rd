% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rouge_wlcs}
\alias{.rouge_wlcs}
\title{.rouge_wlcs}
\usage{
.rouge_wlcs(a, b, alpha)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; its length is taken and its elements indexed.}
\item{alpha}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rouge_native implementation. Called by \code{morie_rouge_w}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
.rouge_wlcs(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, alpha = 0.5)
}
