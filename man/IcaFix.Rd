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
\item{X}{See Usage.}
\item{ncomp}{Defaults to \code{NULL}.}
\item{maxiter}{Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
make components uncorrelated, which is independence only for Gaussians, and a linear mixture tends TOWARDS a Gaussian -- so driving the estimates away from Gaussianity is what unmixes them.  Fixed-point update from Hyvarinen and Oja, Neural Networks 13, 2000, ref [50] of that section.
}
\value{
A list with \code{sources}, \code{unmixing}, \code{mixing}, \code{whitening}, \code{mean}, \code{iterations}, \code{method}.
}
\keyword{internal}
