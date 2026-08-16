% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.blast_best}
\alias{.blast_best}
\title{.blast_best}
\usage{
.blast_best(a, b, score)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; its length is taken and its elements indexed.}
\item{score}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the blastp_native implementation. Called by \code{morie_blastp}. See the file header for the source the module follows.
}
\value{
A list with \code{score}, \code{qi}, \code{sj}, \code{ln}.
}
\keyword{internal}
