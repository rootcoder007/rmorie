% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarimax_fit}
\alias{.sarimax_fit}
\title{.sarimax_fit}
\usage{
.sarimax_fit(y, X = NULL, order = c(0, 1, 1), seasonal_order = c(0, 1, 1), s = 12, include_constant = NULL, method = "ml")
}
\arguments{
\item{y}{See Usage.}
\item{X}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{c(0, 1, 1)}.}
\item{seasonal_order}{Defaults to \code{c(0, 1, 1)}.}
\item{s}{Defaults to \code{12}.}
\item{include_constant}{Defaults to \code{NULL}.}
\item{method}{Defaults to \code{"ml"}.}
}
\description{
Part of the sarimax_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta_se}, \code{estimate}, \code{beta}, \code{phi}, \code{theta}, \code{Phi}, \code{Theta}, \code{ar}, \code{ma}, \code{sigma2}, \code{loglik}, \code{aic}, \code{n_par}, \code{n_used}, \code{residuals}, \code{innovation_variance}, \code{include_constant}, \code{order}, \code{seasonal_order}, \code{s}, \code{fit_method}, \code{method}.
}
\keyword{internal}
