% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.thrtmt_matvec}
\alias{.thrtmt_matvec}
\title{.thrtmt_matvec}
\usage{
.thrtmt_matvec(Z, b)
}
\arguments{
\item{Z}{A matrix; passed to \code{nrow}.}
\item{b}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the thrtmt_native implementation. Called by \code{thrtmt_blip_function}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
