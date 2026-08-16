% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rouge_prf}
\alias{.rouge_prf}
\title{.rouge_prf}
\usage{
.rouge_prf(match, n_cand, n_ref, beta)
}
\arguments{
\item{match}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_cand}{Numeric; combined arithmetically in the body.}
\item{n_ref}{Numeric; combined arithmetically in the body.}
\item{beta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rouge_native implementation. Called by \code{morie_rouge_l}, \code{morie_rouge_n}. See the file header for the source the module follows.
}
\value{
A list with \code{precision}, \code{recall}, \code{f1}.
}
\keyword{internal}
