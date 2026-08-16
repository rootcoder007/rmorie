% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vitdraw}
\alias{.vitdraw}
\title{A deterministic nr-by-nc parameter matrix, row-major off the stream}
\usage{
.vitdraw(nr, nc, skip = 0, scale = 1)
}
\arguments{
\item{nr}{A count; the body uses it as \code{matrix(...)}.}
\item{nc}{A count; the body uses it as \code{matrix(...)}.}
\item{skip}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{scale}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the helpers_vit implementation. Called by \code{Vaean}, \code{Vaeber}, \code{Vaecf} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
