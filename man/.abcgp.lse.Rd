% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.lse}
\alias{.abcgp.lse}
\title{.abcgp.lse}
\usage{
.abcgp.lse(values)
}
\arguments{
\item{values}{A vector; indexed elementwise.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gabc_log_likelihood}, \code{morie_abcgp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.abcgp.lse(V)
}
