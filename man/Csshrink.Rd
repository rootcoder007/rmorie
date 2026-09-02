% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Csshrink}
\alias{Csshrink}
\title{Continuous-shrinkage polygenic effects (Ge et al. 2019)}
\usage{
Csshrink(beta_hat, D, psi, n, sigma2 = 1) 
}
\description{
Continuous-shrinkage polygenic effects (Ge et al. 2019)
}
\arguments{
\item{beta_hat}{Argument \code{beta_hat}; see Usage.}
\item{D}{Argument \code{D}; see Usage.}
\item{psi}{Argument \code{psi}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{sigma2}{Argument \code{sigma2}; see Usage.}
}
\value{
A list with \code{beta}, \code{shrinkage}, \code{n}, \code{sigma2}, \code{n_snp}.
}
\keyword{internal}
\examples{
Csshrink(beta_hat = 5L, D = 5L, psi = 5L, n = 5L)
}
