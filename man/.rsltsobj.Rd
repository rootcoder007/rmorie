% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsltsobj}
\alias{.rsltsobj}
\title{The h smallest squared residuals, their sum, and their indices}
\usage{
.rsltsobj(Xm, yy, th, n, p, h)
}
\arguments{
\item{Xm}{A matrix; indexed by row and column.}
\item{yy}{A vector; indexed elementwise.}
\item{th}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
\item{h}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{Ltsreg}. See the file header for the source the module follows.
}
\value{
A list with \code{tot}, \code{idx}, \code{sq}.
}
\keyword{internal}
