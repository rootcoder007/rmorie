% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cvtmle}
\alias{Cvtmle}
\title{Cross-validated TMLE of the ATE (Zheng & van der Laan 2011)}
\usage{
Cvtmle(y, a, q0, q1, g, fold, n_newton = 50) 
}
\description{
Cross-validated TMLE of the ATE (Zheng & van der Laan 2011)
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{a}{Argument \code{a}; see Usage.}
\item{q0}{Argument \code{q0}; see Usage.}
\item{q1}{Argument \code{q1}; see Usage.}
\item{g}{Argument \code{g}; see Usage.}
\item{fold}{Argument \code{fold}; see Usage.}
\item{n_newton}{Argument \code{n_newton}; see Usage.}
}
\value{
A list with \code{estimate}, \code{se}, \code{psi_fold}, \code{eps_fold}, \code{n_folds}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(3)
n <- 60
a <- rbinom(n, 1, 0.5)
g <- rep(0.5, n)
q1 <- rep(0.6, n); q0 <- rep(0.4, n)
y <- rbinom(n, 1, ifelse(a == 1, 0.6, 0.4))
fold <- rep(1:3, length.out = n)
rmorie:::Cvtmle(y, a, q0, q1, g, fold)
}
