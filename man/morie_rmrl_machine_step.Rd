% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl_machine_step}
\alias{morie_rmrl_machine_step}
\title{(delta_u(u, sigma), delta_r(u, delta_u(u, sigma)))}
\usage{
morie_rmrl_machine_step(machine, u, sigma)
}
\arguments{
\item{machine}{See Usage.}
\item{u}{See Usage.}
\item{sigma}{See Usage.}
}
\description{
Part of the rmrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{u}, \code{reward}.
}
\keyword{internal}
\examples{
morie_rmrl_machine_step(machine = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), u = 5L, sigma = 0.5)
}
