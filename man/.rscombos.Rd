% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rscombos}
\alias{.rscombos}
\title{Lexicographic k-subsets of 1..n, at most cap of them.  Returns a list}
\usage{
.rscombos(n, k, cap = NULL)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{cap}{Defaults to \code{NULL}.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{.rscombosstride}, \code{Lmsreg}, \code{Ltsreg} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{repeat}.
}
\keyword{internal}
\examples{
.rscombos(n = 5L, k = 5L)
}
