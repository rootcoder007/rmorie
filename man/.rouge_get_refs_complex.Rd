% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rouge_get_refs_complex}
\alias{.rouge_get_refs_complex}
\title{.rouge_get_refs_complex}
\usage{
.rouge_get_refs_complex(reference)
}
\arguments{
\item{reference}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rouge_native implementation. Called by \code{morie_rouge_n}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rouge_get_refs_complex(V)
}
