% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_deseq2_xtwx_logdet}
\alias{.ghc_deseq2_xtwx_logdet}
\title{.ghc_deseq2_xtwx_logdet}
\usage{
.ghc_deseq2_xtwx_logdet(X, mu, alpha)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{mu}{A vector; indexed elementwise.}
\item{alpha}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the deseq2_native implementation. Called by \code{cox_reid_loglik}. See the file header for the source the module follows.
}
\value{
The value of \code{ld}, as built in the body.
}
\keyword{internal}
\examples{
.ghc_deseq2_xtwx_logdet(X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), mu = c(1, 2, 3, 4, 5, 6, 7, 8), alpha = 0.5)
}
