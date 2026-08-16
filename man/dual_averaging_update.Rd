% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dual_averaging_update}
\alias{dual_averaging_update}
\title{Equation 6, one iteration of the Nesterov dual-averaging scheme}
\usage{
dual_averaging_update(t, h_bar, log_eps_bar, h_new, mu, gamma = 0.05, t0 = 10, kappa = 0.75)
}
\arguments{
\item{t}{See Usage.}
\item{h_bar}{See Usage.}
\item{log_eps_bar}{See Usage.}
\item{h_new}{See Usage.}
\item{mu}{See Usage.}
\item{gamma}{Defaults to \code{0.05}.}
\item{t0}{Defaults to \code{10}.}
\item{kappa}{Defaults to \code{0.75}.}
}
\description{
for the log step size. Returns the new step size and the running averages carried between calls.
}
\value{
A list with \code{eps}, \code{h_bar}, \code{log_eps_bar}.
}
\keyword{internal}
