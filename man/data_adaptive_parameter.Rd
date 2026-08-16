% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{data_adaptive_parameter}
\alias{data_adaptive_parameter}
\title{data_adaptive_parameter}
\usage{
data_adaptive_parameter(define_on_training, estimate_on_holdout, n, V = 10L, seed = 0L)
}
\arguments{
\item{define_on_training}{Accepted by the signature and not used anywhere in the body.}
\item{estimate_on_holdout}{Accepted by the signature and not used anywhere in the body.}
\item{n}{A count; the body uses it as \code{rep(...)}.}
\item{V}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.}
\item{seed}{Passed to \code{split_sample}. Defaults to \code{0L}.}
}
\description{
A step of the tldapar_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{fold_estimates}, \code{fold_parameters}, \code{se}, \code{ci}, \code{V}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
x <- rnorm(50, 1)
def <- function(train) mean(x[train]) > 1
est <- function(p, hold) {
  v <- if (p) x[hold] else -x[hold]
  list(estimate = mean(v), ic = v - mean(v))
}
r <- data_adaptive_parameter(def, est, n = 50, V = 5)
str(r, max.level = 1)
}
