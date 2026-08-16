% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.copynm_shuffle}
\alias{.copynm_shuffle}
\title{In-place Fisher-Yates shuffle with one .ghc_unif draw per swap}
\usage{
.copynm_shuffle(w, e)
}
\arguments{
\item{w}{A vector; its length is taken and its elements indexed.}
\item{e}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the copynm_native implementation. Called by \code{.copynm_binary_supported}, \code{copynm}. See the file header for the source the module follows.
}
\value{
Invisibly,the value of \code{w}, as built in the body.
}
\keyword{internal}
