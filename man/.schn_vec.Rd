% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schn_vec}
\alias{.schn_vec}
\title{.schn_vec}
\usage{
.schn_vec(x)
}
\arguments{
\item{x}{A list; the body checks with \code{is.list}.}
}
\description{
A step of the schN_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schn_vec(V)
}
