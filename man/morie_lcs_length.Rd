% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_lcs_length}
\alias{morie_lcs_length}
\title{morie_lcs_length}
\usage{
morie_lcs_length(a, b)
}
\arguments{
\item{a}{Passed to \code{.rouge_lcs_length}.}
\item{b}{Passed to \code{.rouge_lcs_length}.}
}
\description{
A step of the rouge_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.rouge_lcs_length}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_lcs_length(V, V)
}
