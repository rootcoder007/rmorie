% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpatialPca}
\alias{SpatialPca}
\title{MULTISPATI: diagonalise H = (1/n) X\' ((W + W\')/2) X on the centred,}
\usage{
SpatialPca(x, w, naxes = 2L) 
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{w}{Passed to \code{.morie_spx_chkw}.}
\item{naxes}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{2L}.}
}
\description{
unit-variance X, so an axis is scored by SPATIAL covariance, not variance.  Eigenvalues may be NEGATIVE -- that is a local-contrast axis, which ordinary PCA cannot express -- and are returned signed. W is symmetrised first: a row-standardised W is ASYMMETRIC and a symmetric eigensolver would read one triangle only.  Dray, Said & Debias (2008).  NOT in Schabenberger & Gotway.
}
\value{
A list with \code{eigenvalues}, \code{loadings}, \code{scores}, \code{lagged_scores}, \code{total_variance}, \code{eigenvalues_may_be_negative}, \code{weights_symmetrised}, \code{naxes}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
W <- matrix(0, 10, 10)
W[abs(row(W) - col(W)) == 1] <- 1
X <- matrix(rnorm(30), 10, 3)
str(SpatialPca(X, W), max.level = 1)
}
