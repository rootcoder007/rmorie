% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03drdid}
\alias{.s03drdid}
\title{Doubly robust DiD for panel data, Sant\\\'Anna and Zhao (2020) eq.}
\usage{
.s03drdid(dy, D, X = NULL, weights = NULL)
}
\arguments{
\item{dy}{See Usage.}
\item{D}{See Usage.}
\item{X}{Defaults to \code{NULL}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
(2.6): tau = E[(w1(D) - w0(D, X; pi)) (dY - mu_0(X))] w1 = D / E[D] w0 = [pi(X)(1-D)/(1-pi(X))] / E[pi(X)(1-D)/(1-pi(X))]
}
\value{
A list with \code{tau}, \code{inf}, \code{se}, \code{pi}, \code{mu0}, \code{w1}, \code{w0}, \code{gamma}, \code{beta0}.
}
\keyword{internal}
