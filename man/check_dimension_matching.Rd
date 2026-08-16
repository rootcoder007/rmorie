% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{check_dimension_matching}
\alias{check_dimension_matching}
\title{check_dimension_matching}
\usage{
check_dimension_matching(models, moves)
}
\arguments{
\item{models}{A vector; its length is taken and its elements indexed.}
\item{moves}{See Usage.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}. See the file header for the source the module follows.
}
\value{
The value of \code{by_pair}, as built in the body.
}
\keyword{internal}
\examples{
models <- list(m1 = list(dim = 1L, logpost = function(th) -sum(th^2)),
               m2 = list(dim = 2L, logpost = function(th) -sum(th^2)))
moves <- list(
  list(frm = "m1", to = "m2", n_u = 1L, n_u_rev = 0L,
       propose = function(theta, uni) qnorm(uni()),
       transform = function(theta, u) list(c(theta, u), numeric(0))),
  list(frm = "m2", to = "m1", n_u = 0L, n_u_rev = 1L,
       propose = function(theta, uni) numeric(0),
       transform = function(theta, u) list(theta[1], theta[2]))
)
names(check_dimension_matching(models, moves))
}
