% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lyapunov_exponent}
\alias{lyapunov_exponent}
\title{lyapunov_exponent}
\usage{
lyapunov_exponent(y, embedding = NULL, tau = NULL, dt = 1, fit = NULL, min_sep = NULL, max_steps = NULL, method = "rosenstein", k = NULL)
}
\arguments{
\item{y}{See Usage.}
\item{embedding}{Defaults to \code{NULL}.}
\item{tau}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{1}.}
\item{fit}{Defaults to \code{NULL}.}
\item{min_sep}{Defaults to \code{NULL}.}
\item{max_steps}{Defaults to \code{NULL}.}
\item{method}{Defaults to \code{"rosenstein"}.}
\item{k}{Defaults to \code{NULL}.}
}
\description{
Part of the lyapun_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{lambda1}, \code{rosenstein}, \code{sato}, \code{sato_k}, \code{sato_k_curve}, \code{se}, \code{r_squared}, \code{intercept}, \code{time}, \code{log_divergence}, \code{log_ratio}, \code{n_pairs}, \code{fit_range}, \code{k}, \code{m}, \code{tau}, \code{min_sep}, \code{n_points}, \code{n}, \code{dt}, \code{method}, \code{note}.
}
\keyword{internal}
