% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_aft_result}
\alias{.morie_aft_result}
\title{.morie_aft_result}
\usage{
.morie_aft_result(t, e, X, fit, family, title, method)
}
\arguments{
\item{t}{A vector; its length is taken.}
\item{e}{Numeric; passed to \code{sum}.}
\item{X}{Carried through into a list the body builds.}
\item{fit}{A list; the body reads \code{$beta}, \code{$converged}, \code{$cov}, \code{$log_sigma}, \code{$loglik}, \code{$n_iter} from it.}
\item{family}{Carried through into a list the body builds.}
\item{title}{See Usage.}
\item{method}{Carried through into a list the body builds.}
}
\description{
A step of the aft_native implementation. Called by \code{.morie_aft_common}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{se}, \code{time_ratio}, \code{sigma}, \code{log_sigma}, \code{loglik}, \code{aic}, \code{family}, \code{n}, \code{n_events}, \code{n_iter}, \code{converged}, \code{cov}, \code{time}, \code{event}, \code{X}, \code{method}.
}
\keyword{internal}
