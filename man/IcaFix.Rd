% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IcaFix}
\alias{IcaFix}
\title{Section 9.7.2: model eq (9.43), unmixing eq (9.44).  PCA can only}
\usage{
IcaFix(X, ncomp = NULL, maxiter = 200, tol = 1e-08, seed = 1) 
}
\arguments{
\item{X}{Passed to \code{.morie_bx_mat}.}
\item{ncomp}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{maxiter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-08}.}
\item{seed}{Passed to \code{.morie_bx_rng}. Defaults to \code{1}.}
}
\description{
make components uncorrelated, which is independence only for Gaussians, and a linear mixture tends TOWARDS a Gaussian -- so driving the estimates away from Gaussianity is what unmixes them.  Fixed-point update from Hyvarinen and Oja, Neural Networks 13, 2000, ref [50] of that section.
}
\value{
A list with \code{sources}, \code{unmixing}, \code{mixing}, \code{whitening}, \code{mean}, \code{iterations}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(4)
S <- rbind(sin(2 * pi * (1:300) / 25), sign(sin(2 * pi * (1:300) / 60)))
A <- matrix(c(1, 0.5, 0.4, 1), 2, 2)
X <- A \%*\% S + 0.02 * matrix(rnorm(600), 2, 300)
r <- IcaFix(X)
str(r, max.level = 1)
}
