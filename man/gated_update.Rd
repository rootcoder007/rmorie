% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gated_update}
\alias{gated_update}
\title{gated_update}
\usage{
gated_update(s, v, U, V, phi) 
}
\arguments{
\item{s}{Passed to \code{.painn_vec}.}
\item{v}{Passed to \code{.painn_mat}.}
\item{U}{A matrix; passed to \code{t}.}
\item{V}{A matrix; passed to \code{t}.}
\item{phi}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the painn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ds}, \code{dv}, \code{scalar_from_vectors}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
F <- 4; D <- 3
s <- rnorm(F)
v <- matrix(rnorm(D * F), D, F)
U <- diag(F); V <- diag(F)
phi <- function(sv, dot, nrm) list(ds = sv + dot, gate = rep(0.5, length(sv)))
u <- gated_update(s, v, U, V, phi)
c(length(u$ds), nrow(u$dv))
}
