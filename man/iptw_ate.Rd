% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{iptw_ate}
\alias{iptw_ate}
\title{ATE by weighted regression of Y on A --------------------------}
\usage{
iptw_ate(y, A, H, library = NULL, n_folds = 10, meta = "nnls", trim = 0.01, 
    ridge = 1e-08, level = 0.95) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{H}{Passed to \code{flexible_iptw}.}
\item{library}{Passed to \code{flexible_iptw}.}
\item{n_folds}{Passed to \code{flexible_iptw}. Defaults to \code{10}.}
\item{meta}{Passed to \code{flexible_iptw}. Defaults to \code{"nnls"}.}
\item{trim}{Passed to \code{flexible_iptw}. Defaults to \code{0.01}.}
\item{ridge}{Passed to \code{flexible_iptw}. Defaults to \code{1e-08}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
}
\description{
A step of the flxipt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{ci}, \code{mean_treated}, \code{mean_control}, \code{propensity}, \code{weights}, \code{sl_weights}, \code{cv_risk}, \code{best_candidate}, \code{max_weight}, \code{min_propensity}, \code{n}, \code{level}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 60
H <- matrix(rnorm(n * 2), n, 2)
A <- rbinom(n, 1, plogis(0.5 * H[, 1]))
y <- 1 + 0.8 * A + H[, 1] + rnorm(n, 0, 0.3)
r <- iptw_ate(y, A, H, n_folds = 4)
str(r, max.level = 1)
}
