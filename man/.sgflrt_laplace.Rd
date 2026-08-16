% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_laplace}
\alias{.sgflrt_laplace}
\title{Inner Laplace mode and the approximated log likelihood. The random}
\usage{
.sgflrt_laplace(y, X, Sig, family, inner_iter, tol, disp = 1)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{Sig}{See Usage.}
\item{family}{See Usage.}
\item{inner_iter}{See Usage.}
\item{tol}{See Usage.}
\item{disp}{Defaults to \code{1}.}
}
\description{
effect is carried as u = Lv with LL\\\' = Sigma and v ~ N(0, I), so the penalty is v\\\'v/2 and Sigma^-1 is never formed. That is not a nicety: a spatial correlation matrix with any two nearby locations is close to singular, an explicitly inverted one is dominated by the jitter that made the inversion possible, and the fitted coefficients then miss the exact Gaussian answer by tenths rather than by 1e-10.
}
\value{
A list with \code{lap}, \code{beta}, \code{u}, \code{mu}, \code{eta}, \code{loglik}, \code{w}, \code{L}, \code{v}.
}
\keyword{internal}
