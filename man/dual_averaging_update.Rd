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
\item{t}{Numeric; passed to \code{sqrt}.}
\item{h_bar}{Numeric; combined arithmetically in the body.}
\item{log_eps_bar}{Numeric; combined arithmetically in the body.}
\item{h_new}{Numeric; combined arithmetically in the body.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.05}.}
\item{t0}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
\item{kappa}{Numeric; combined arithmetically in the body. Defaults to \code{0.75}.}
}
\description{
for the log step size. Returns the new step size and the running averages carried between calls.
}
\value{
A list with \code{eps}, \code{h_bar}, \code{log_eps_bar}.
}
\keyword{internal}
\examples{
dual_averaging_update(t = 5L, h_bar = c(1, 2, 3, 4, 5, 6, 7, 8), log_eps_bar = c(1, 2, 3, 4, 5, 6, 7, 8), h_new = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8))
}
