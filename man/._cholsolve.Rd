% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{._cholsolve}
\alias{._cholsolve}
\title{._cholsolve}
\usage{
._cholsolve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
\item{b}{A matrix; passed to \code{solve}.}
}
\description{
A step of the hierF_native implementation. Called by \code{mint_P}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
