% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{divergence_curve}
\alias{divergence_curve}
\title{divergence_curve}
\usage{
divergence_curve(y, m = NULL, tau = NULL, dt = 1, min_sep = NULL, max_steps = NULL)
}
\arguments{
\item{y}{See Usage.}
\item{m}{Defaults to \code{NULL}.}
\item{tau}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{1}.}
\item{min_sep}{Defaults to \code{NULL}.}
\item{max_steps}{Defaults to \code{NULL}.}
}
\description{
Part of the lyapun_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{time}, \code{log_divergence}, \code{log_ratio}, \code{n_pairs}, \code{neighbour}, \code{d0}, \code{points}, \code{m}, \code{tau}, \code{min_sep}, \code{n_points}, \code{n_obs}.
}
\keyword{internal}
