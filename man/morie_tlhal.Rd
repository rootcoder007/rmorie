% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlhal}
\alias{morie_tlhal}
\title{morie_tlhal}
\usage{
morie_tlhal(X, y, lambdas = NULL, V = 5L, seed = 0L, lam = 1, iters = 2000L, step = 0.05, max_order = 2L, knots = NULL, intercept = TRUE, mode = c("fit", "predict", "cv", "norm"))
}
\arguments{
\item{X}{Passed to \code{cv_select_lambda}.}
\item{y}{Passed to \code{cv_select_lambda}.}
\item{lambdas}{Passed to \code{cv_select_lambda}.}
\item{V}{Passed to \code{cv_select_lambda}. Defaults to \code{5L}.}
\item{seed}{Passed to \code{cv_select_lambda}. Defaults to \code{0L}.}
\item{lam}{Passed to \code{hal_fit}. Defaults to \code{1}.}
\item{iters}{Passed to \code{cv_select_lambda}. Defaults to \code{2000L}.}
\item{step}{Passed to \code{cv_select_lambda}. Defaults to \code{0.05}.}
\item{max_order}{Passed to \code{cv_select_lambda}. Defaults to \code{2L}.}
\item{knots}{Passed to \code{hal_fit}.}
\item{intercept}{Passed to \code{cv_select_lambda}. Defaults to \code{TRUE}.}
\item{mode}{One of \code{"cv"}, \code{"norm"}, \code{"predict"}.}
}
\description{
A step of the tlhal_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{hal_fit}.
}
\keyword{internal}
