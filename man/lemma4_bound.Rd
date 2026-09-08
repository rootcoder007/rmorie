% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lemma4_bound}
\alias{lemma4_bound}
\title{lemma4_bound}
\usage{
lemma4_bound(counts, gamma) 
}
\arguments{
\item{counts}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{gamma}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the pate_native implementation. Called by \code{moments_accountant}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
lemma4_bound(counts = c(1, 2, 3, 4, 5, 6, 7, 8), gamma = 0.5)
}
