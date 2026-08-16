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
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{lambdas}{Defaults to \code{NULL}.}
\item{V}{Defaults to \code{5L}.}
\item{seed}{Defaults to \code{0L}.}
\item{lam}{Defaults to \code{1}.}
\item{iters}{Defaults to \code{2000L}.}
\item{step}{Defaults to \code{0.05}.}
\item{max_order}{Defaults to \code{2L}.}
\item{knots}{Defaults to \code{NULL}.}
\item{intercept}{Defaults to \code{TRUE}.}
\item{mode}{Defaults to \code{c("fit", "predict", "cv", "norm")}.}
}
\description{
Part of the tlhal_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{hal_fit}.
}
\keyword{internal}
