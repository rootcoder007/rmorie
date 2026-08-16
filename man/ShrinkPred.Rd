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
\item{y}{See Usage.}
\item{cluster}{See Usage.}
\item{sigma2_u}{See Usage.}
\item{sigma2_e}{See Usage.}
}
\description{
lambda_j = sigma2_e / (sigma2_e + n_j sigma2_u).  lambda depends on the CLUSTER\\'S OWN SIZE; a common lambda over-shrinks the large clusters. Stein (1956); Morris (1983) JASA 78:47-55.  NOT in Schabenberger & Gotway -- a fixed-string search for "shrinkage" returns nothing.
}
\value{
A list with \code{clusters}, \code{shrunk}, \code{raw}, \code{lambda}, \code{sizes}, \code{grand_mean}, \code{sigma2_u}, \code{sigma2_e}, \code{shrinkage_depends_on_cluster_size}, \code{n}, \code{method}.
}
\keyword{internal}
