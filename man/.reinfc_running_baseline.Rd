% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_running_baseline}
\alias{.reinfc_running_baseline}
\title{.reinfc_running_baseline}
\usage{
.reinfc_running_baseline(state, baseline, gamma)
}
\arguments{
\item{state}{A list; the body reads \code{$v1}, \code{$v2} from it.}
\item{baseline}{One of \code{"comparison"}, \code{"none"}.}
\item{gamma}{See Usage.}
}
\description{
A step of the reinfc_native implementation. Called by \code{.reinfc_run_bernoulli}, \code{.reinfc_run_gaussian}, \code{.reinfc_run_logistic}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
