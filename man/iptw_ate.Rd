% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{iptw_ate}
\alias{iptw_ate}
\title{ATE by weighted regression of Y on A --------------------------}
\usage{
iptw_ate(y, A, H, library = NULL, n_folds = 10, meta = "nnls", trim = 0.01, ridge = 1e-08, level = 0.95)
}
\arguments{
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{H}{See Usage.}
\item{library}{Defaults to \code{NULL}.}
\item{n_folds}{Defaults to \code{10}.}
\item{meta}{Defaults to \code{"nnls"}.}
\item{trim}{Defaults to \code{0.01}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{level}{Defaults to \code{0.95}.}
}
\description{
Part of the flxipt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{ci}, \code{mean_treated}, \code{mean_control}, \code{propensity}, \code{weights}, \code{sl_weights}, \code{cv_risk}, \code{best_candidate}, \code{max_weight}, \code{min_propensity}, \code{n}, \code{level}, \code{method}.
}
\keyword{internal}
