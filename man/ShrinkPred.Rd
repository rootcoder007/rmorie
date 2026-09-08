% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ShrinkPred}
\alias{ShrinkPred}
\title{Thetahat_j = ybar.. + (1 - lambda_j)(ybar_j - ybar..),}
\usage{
ShrinkPred(y, cluster, sigma2_u, sigma2_e) 
}
\arguments{
\item{y}{Passed to \code{.morie_spx_chkv}.}
\item{cluster}{Passed to \code{.morie_spx_chkv}.}
\item{sigma2_u}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma2_e}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
lambda_j = sigma2_e / (sigma2_e + n_j sigma2_u).  lambda depends on the CLUSTER\'S OWN SIZE; a common lambda over-shrinks the large clusters. Stein (1956); Morris (1983) JASA 78:47-55.  NOT in Schabenberger & Gotway -- a fixed-string search for "shrinkage" returns nothing.
}
\value{
A list with \code{clusters}, \code{shrunk}, \code{raw}, \code{lambda}, \code{sizes}, \code{grand_mean}, \code{sigma2_u}, \code{sigma2_e}, \code{shrinkage_depends_on_cluster_size}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(4)
y <- c(rnorm(5, 1), rnorm(8, 2), rnorm(3, 0))
cl <- c(rep(0, 5), rep(1, 8), rep(2, 3))
ShrinkPred(y, cl, sigma2_u = 0.5, sigma2_e = 1)
}
