% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_combn}
\alias{.morie_ml_combn}
\title{.morie_ml_combn}
\usage{
.morie_ml_combn(seq_, k)
}
\arguments{
\item{seq_}{A vector; its length is taken and its elements indexed.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the causal_molak implementation. Called by \code{morie_sepset}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.morie_ml_combn(seq_ = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
