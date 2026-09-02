% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{seasonal_harmonics}
\alias{seasonal_harmonics}
\title{seasonal_harmonics}
\usage{
seasonal_harmonics(m, k = NULL) 
}
\arguments{
\item{m}{Numeric; combined arithmetically in the body.}
\item{k}{Optional; may be \code{NULL}. A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the bats_native implementation. Called by \code{bats_filter}, \code{BatsSpec}, \code{morie_bats}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
seasonal_harmonics(m = 5L)
}
