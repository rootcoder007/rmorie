% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kvmse_codebook}
\alias{.kvmse_codebook}
\title{.kvmse_codebook}
\usage{
.kvmse_codebook(b, iters = 200, grid = 2001, lo = -6, hi = 6)
}
\arguments{
\item{b}{Coerced to integer by the body, with \code{as.integer}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{grid}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2001}.}
\item{lo}{Numeric; combined arithmetically in the body. Defaults to \code{-6}.}
\item{hi}{Numeric; combined arithmetically in the body. Defaults to \code{6}.}
}
\description{
A step of the tqkmse implementation. Called by \code{Kvmse}, \code{Kvquant}, \code{Vcquant}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.kvmse_codebook(V)
}
