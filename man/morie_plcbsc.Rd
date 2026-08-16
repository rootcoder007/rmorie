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
\item{y_treated}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y_donors}{Iterated over elementwise, with \code{lapply}.}
\item{t0}{A count; the body uses it as \code{seq_len(...)}.}
\item{x_treated}{Passed to \code{fit}.}
\item{x_donors}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{v}{Carried through into a list the body builds.}
\item{statistic}{One of \code{"effect"}, \code{"rmspe_ratio"}. Defaults to \code{"effect"}.}
\item{...}{Passed through.}
}
\description{
A step of the plcbsc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{gaps}, \code{weights}, \code{fit_loss}, \code{placebo}, \code{pvalue}, \code{rank}, \code{n_donors}, \code{t0}, \code{statistic}, \code{rmspe_pre}, \code{rmspe_post}, \code{note}, \code{method}.
}
\keyword{internal}
