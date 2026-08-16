% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_interp}
\alias{.morie_jo_interp}
\title{.morie_jo_interp}
\usage{
.morie_jo_interp(theta, length_)
}
\arguments{
\item{theta}{A vector; its length is taken and its elements indexed.}
\item{length_}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the ts_joseph implementation. Called by \code{morie_nhitsnet}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
