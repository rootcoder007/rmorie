% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_remlfn}
\alias{morie_remlfn}
\title{morie_remlfn}
\usage{
morie_remlfn(y, group, tol = 1e-10, max_iter = 5000, solver = "auto") 
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{group}{A vector; its length is taken.}
\item{tol}{Passed to \code{.remlfn_nelder_mead}. Defaults to \code{1e-10}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5000}.}
\item{solver}{One of \code{"auto"}, \code{"closed"}, \code{"optim"}. Defaults to \code{"auto"}.}
}
\description{
A step of the remlfn_native implementation. Called by \code{morie_vcomp}. See the file header for the source the module follows.
}
\value{
A list with \code{sigma2_a}, \code{sigma2_e}, \code{mu}, \code{loglik}, \code{n_iter}, \code{converged}, \code{icc}, \code{a}, \code{N}, \code{closed_form}, \code{solver}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_remlfn(y = rnorm(10), group = rbinom(10, 1, 0.5)); TRUE
}
