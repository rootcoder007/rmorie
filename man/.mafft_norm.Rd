% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_norm}
\alias{.mafft_norm}
\title{.mafft_norm}
\usage{
.mafft_norm(vals)
}
\arguments{
\item{vals}{A vector; its length is taken.}
}
\description{
A step of the mafft_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mafft_norm(V)
}
