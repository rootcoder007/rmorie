% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rouge_counts}
\alias{.rouge_counts}
\title{.rouge_counts}
\usage{
.rouge_counts(keys)
}
\arguments{
\item{keys}{A vector; its length is taken.}
}
\description{
A step of the rouge_native implementation. Called by \code{morie_rouge_n}. See the file header for the source the module follows.
}
\value{
The value of \code{table}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rouge_counts(V)
}
