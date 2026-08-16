% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fit_spec}
\alias{.fit_spec}
\title{.fit_spec}
\usage{
.fit_spec(y, spec, long_run_b = 0, maxiter = 2000)
}
\arguments{
\item{y}{Passed to \code{box_cox}.}
\item{spec}{A list; the body reads \code{$use_box_cox} from it.}
\item{long_run_b}{Passed to \code{fit_seed_state}. Defaults to \code{0}.}
\item{maxiter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.}
}
\description{
A step of the bats_native implementation. Called by \code{morie_bats}. See the file header for the source the module follows.
}
\value{
A list with \code{theta}, \code{x0}, \code{resid}, \code{fitted}, \code{loglik}, \code{omega}, \code{aic}, \code{n_par}.
}
\keyword{internal}
