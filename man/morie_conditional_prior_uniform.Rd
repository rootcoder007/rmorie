% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_conditional_prior_uniform}
\alias{morie_conditional_prior_uniform}
\title{morie_conditional_prior_uniform}
\usage{
morie_conditional_prior_uniform(theta_set, n_grid = 401L)
}
\arguments{
\item{theta_set}{A list; the body reads \code{$lower}, \code{$upper} from it.}
\item{n_grid}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{401L}.}
}
\description{
A step of the bndbye_native implementation. Called by \code{morie_posterior_hpd}. See the file header for the source the module follows.
}
\value{
A list with \code{grid}, \code{density}.
}
\keyword{internal}
\examples{
r <- morie_conditional_prior_uniform(list(lower = 0.2, upper = 0.8),
                                     n_grid = 21L)
str(r, max.level = 1)
}
