% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_plcbsc}
\alias{morie_plcbsc}
\title{morie_plcbsc}
\usage{
morie_plcbsc(y_treated, y_donors, t0, x_treated = NULL, x_donors = NULL, v = NULL, statistic = "effect", ...)
}
\arguments{
\item{y_treated}{See Usage.}
\item{y_donors}{See Usage.}
\item{t0}{See Usage.}
\item{x_treated}{Defaults to \code{NULL}.}
\item{x_donors}{Defaults to \code{NULL}.}
\item{v}{Defaults to \code{NULL}.}
\item{statistic}{Defaults to \code{"effect"}.}
\item{...}{Passed through.}
}
\description{
Part of the plcbsc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{gaps}, \code{weights}, \code{fit_loss}, \code{placebo}, \code{pvalue}, \code{rank}, \code{n_donors}, \code{t0}, \code{statistic}, \code{rmspe_pre}, \code{rmspe_post}, \code{note}, \code{method}.
}
\keyword{internal}
