% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_package}
\alias{.sarima_package}
\title{.sarima_package}
\usage{
.sarima_package(y, w, phi, theta, Phi, Theta, s, order, seasonal_order, ll, cs, method, res)
}
\arguments{
\item{y}{See Usage.}
\item{w}{See Usage.}
\item{phi}{See Usage.}
\item{theta}{See Usage.}
\item{Phi}{See Usage.}
\item{Theta}{See Usage.}
\item{s}{See Usage.}
\item{order}{See Usage.}
\item{seasonal_order}{See Usage.}
\item{ll}{See Usage.}
\item{cs}{See Usage.}
\item{method}{See Usage.}
\item{res}{See Usage.}
}
\description{
Part of the sarima_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{sigma2}, \code{phi}, \code{theta}, \code{Phi}, \code{Theta}, \code{ar}, \code{ma}, \code{loglik}, \code{aic}, \code{n_used}, \code{n_par}, \code{residuals}, \code{ssq}, \code{order}, \code{seasonal_order}, \code{s}, \code{y}, \code{w}, \code{fit_method}, \code{converged}, \code{method}.
}
\keyword{internal}
