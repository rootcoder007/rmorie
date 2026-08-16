% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_deseq2_nb_loglik}
\alias{.ghc_deseq2_nb_loglik}
\title{.ghc_deseq2_nb_loglik}
\usage{
.ghc_deseq2_nb_loglik(K, mu, alpha)
}
\arguments{
\item{K}{A vector; its length is taken and its elements indexed.}
\item{mu}{A vector; indexed elementwise.}
\item{alpha}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the deseq2_native implementation. Called by \code{cox_reid_loglik}. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
