% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.k03_golden}
\alias{.k03_golden}
\title{# Deterministic golden-section maximisation on [lo, hi]}
\usage{
.k03_golden(f, lo, hi, iters = 80L)
}
\arguments{
\item{f}{See Usage.}
\item{lo}{Numeric; combined arithmetically in the body.}
\item{hi}{Numeric; combined arithmetically in the body.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{80L}.}
}
\description{
A step of the k03volsv implementation. Called by \code{Volsv}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
