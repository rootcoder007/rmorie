% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{loglik_profile}
\alias{loglik_profile}
\title{loglik_profile}
\usage{
loglik_profile(y, grid, make_model, n_particles = 200L, n_reps = 5L, seed = 0L)
}
\arguments{
\item{y}{See Usage.}
\item{grid}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{make_model}{Accepted by the signature and not used anywhere in the body.}
\item{n_particles}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{n_reps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0L}.}
}
\description{
A step of the pftrep_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mle}, \code{grid}, \code{loglik}, \code{se}, \code{max_loglik}, \code{n_particles}, \code{n_reps}, \code{method}.
}
\keyword{internal}
