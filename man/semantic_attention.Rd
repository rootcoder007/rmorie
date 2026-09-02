% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{semantic_attention}
\alias{semantic_attention}
\title{semantic_attention}
\usage{
semantic_attention(Z_per_metapath, W, b, q) 
}
\arguments{
\item{Z_per_metapath}{A vector; indexed elementwise.}
\item{W}{A matrix; passed to \code{\%*\%}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{q}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the hetgnn_native implementation. Called by \code{han_forward}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{scores}, \code{metapaths}, \code{note}.
}
\keyword{internal}
\examples{
semantic_attention(Z_per_metapath = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), W = 5L, b = 5L, q = 0.5)
}
