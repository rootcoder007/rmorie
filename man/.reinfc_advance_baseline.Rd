% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_advance_baseline}
\alias{.reinfc_advance_baseline}
\title{.reinfc_advance_baseline}
\usage{
.reinfc_advance_baseline(state, baseline, gamma, r)
}
\arguments{
\item{state}{A list; the body reads \code{$v1}, \code{$v2} from it.}
\item{baseline}{One of \code{"comparison"}, \code{"mean"}.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{r}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the reinfc_native implementation. Called by \code{.reinfc_run_bernoulli}, \code{.reinfc_run_gaussian}, \code{.reinfc_run_logistic}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.reinfc_advance_baseline(state = c(1, 2, 3, 4, 5, 6, 7, 8), baseline = 5L, gamma = 0.5, r = c(1, 2, 3, 4, 5, 6, 7, 8))
}
