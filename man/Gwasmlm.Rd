% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Gwasmlm}
\alias{Gwasmlm}
\title{Unified mixed-model per-SNP association test (Yu et al. 2006)}
\usage{
Gwasmlm(y, X, snp, Vinv) 
}
\description{
Unified mixed-model per-SNP association test (Yu et al. 2006)
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{snp}{Argument \code{snp}; see Usage.}
\item{Vinv}{Argument \code{Vinv}; see Usage.}
}
\value{
A list with \code{beta}, \code{se}, \code{statistic}, \code{df}, \code{coefficients}, \code{sigma2}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 30
X <- cbind(1, rnorm(n))
snp <- rbinom(n, 2, 0.3)
y <- X \%*\% c(1, 0.5) + 0.4 * snp + rnorm(n, 0, 0.5)
Gwasmlm(as.numeric(y), X, snp, diag(n))
}
