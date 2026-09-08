% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_blast_pvalue}
\alias{morie_blast_pvalue}
\title{morie_blast_pvalue}
\usage{
morie_blast_pvalue(score, m, n, lam, K, c = 1L) 
}
\arguments{
\item{score}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Numeric; combined arithmetically in the body.}
\item{K}{Numeric; combined arithmetically in the body.}
\item{c}{Numeric; combined arithmetically in the body. Defaults to \code{1L}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_blstn}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_blast_pvalue(score = 35, m = 1000, n = 1e6, lam = 0.192,
                   K = 0.176)
}
