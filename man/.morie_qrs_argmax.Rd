% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_argmax}
\alias{.morie_qrs_argmax}
\title{0-based argmax over the half-open range [lo, hi); first max on ties,}
\usage{
.morie_qrs_argmax(v, lo, hi)
}
\arguments{
\item{v}{A vector; indexed elementwise.}
\item{lo}{Numeric; combined arithmetically in the body.}
\item{hi}{See Usage.}
}
\description{
exactly as Python\\'s max(range(...), key=...).
}
\value{
A numeric value.
}
\keyword{internal}
