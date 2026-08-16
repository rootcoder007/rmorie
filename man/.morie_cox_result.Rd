% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_result}
\alias{.morie_cox_result}
\title{.morie_cox_result}
\usage{
.morie_cox_result(t, e, X, fit, label, method)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{e}{Numeric; passed to \code{sum}.}
\item{X}{See Usage.}
\item{fit}{A list; the body reads \code{$beta}, \code{$converged}, \code{$I}, \code{$loglik}, \code{$n_iter} from it.}
\item{label}{Character; passed to \code{tolower}.}
\item{method}{See Usage.}
}
\description{
A step of the cox_native implementation. Called by \code{morie_breslow_tie_correction}, \code{morie_efron_tie_correction}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{se}, \code{z}, \code{p_value}, \code{hazard_ratio}, \code{loglik}, \code{cov}, \code{information}, \code{n_ties}, \code{n_events}, \code{n}, \code{n_iter}, \code{converged}, \code{ties}, \code{time}, \code{event}, \code{X}, \code{method}.
}
\keyword{internal}
