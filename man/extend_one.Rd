% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{extend_one}
\alias{extend_one}
\title{extend_one}
\usage{
extend_one(qchars, schars, qi, si, w, sc, X) 
}
\arguments{
\item{qchars}{A vector; its length is taken and its elements indexed.}
\item{schars}{A vector; its length is taken and its elements indexed.}
\item{qi}{Numeric; combined arithmetically in the body.}
\item{si}{Numeric; combined arithmetically in the body.}
\item{w}{A count; the body uses it as \code{seq_len(...)}.}
\item{sc}{Accepted by the signature and not used anywhere in the body.}
\item{X}{Passed to \code{>}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_blstn}. See the file header for the source the module follows.
}
\value{
A list with \code{score}, \code{qs}, \code{ss}, \code{length}.
}
\keyword{internal}
\examples{
q <- strsplit("ACGTACGT", "")[[1]]
s <- strsplit("TTACGTAA", "")[[1]]
sc <- function(a, b) if (a == b) 5 else -4
r <- extend_one(q, s, qi = 0L, si = 2L, w = 4L, sc, X = 20)
str(r, max.level = 1)
}
