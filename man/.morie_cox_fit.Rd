% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_fit}
\alias{.morie_cox_fit}
\title{.morie_cox_fit}
\usage{
.morie_cox_fit(t, e, X, ties = "efron", max_iter = 50L, tol = 1e-09, offset = NULL)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{e}{A vector; indexed elementwise.}
\item{X}{A matrix; indexed by row and column.}
\item{ties}{Passed to \code{.morie_cox_score}. Defaults to \code{"efron"}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-09}.}
\item{offset}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the cox_native implementation. Called by \code{morie_breslow_tie_correction}, \code{morie_cause_specific_hazard}, \code{morie_cox_breslow_step} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{loglik}, \code{I}, \code{U}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
\examples{
.morie_cox_fit(t = 5L, e = c(1, 2, 3, 4, 5, 6, 7, 8), X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
