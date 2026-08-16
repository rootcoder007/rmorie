% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{flexible_iptw}
\alias{flexible_iptw}
\title{IPTW: propensity by Super Learner, weights A/g + (1-A)/(1-g)}
\usage{
flexible_iptw(A, H, library = NULL, n_folds = 10, meta = "nnls", trim = 0.01, ridge = 1e-08, stabilize = FALSE)
}
\arguments{
\item{A}{See Usage.}
\item{H}{See Usage.}
\item{library}{Defaults to \code{NULL}.}
\item{n_folds}{Defaults to \code{10}.}
\item{meta}{Defaults to \code{"nnls"}.}
\item{trim}{Defaults to \code{0.01}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{stabilize}{Defaults to \code{FALSE}.}
}
\description{
Part of the flxipt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{propensity}, \code{weights}, \code{estimate}, \code{sl_weights}, \code{cv_risk}, \code{cv_risk_ensemble}, \code{best_candidate}, \code{max_weight}, \code{min_propensity}, \code{max_propensity}, \code{n}, \code{trim}, \code{stabilized}, \code{library}, \code{method}.
}
\keyword{internal}
