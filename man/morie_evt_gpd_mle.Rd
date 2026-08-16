% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_gpd_mle}
\alias{morie_evt_gpd_mle}
\title{GPD maximum-likelihood fit (Coles 2001 sec. 4.3.2)}
\usage{
morie_evt_gpd_mle(y)
}
\arguments{
\item{y}{threshold excesses}
}
\description{
GPD maximum-likelihood fit (Coles 2001 sec. 4.3.2)
}
\value{
A list with \code{sigma}, \code{xi}, \code{loglik}, \code{cov}, \code{n}, \code{converged}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_evt_gpd_mle(V)
}
