% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vcomp_f_ppf}
\alias{.vcomp_f_ppf}
\title{Monotone bisection on the CDF (same convention as the R arm)}
\usage{
.vcomp_f_ppf(p, d1, d2, iters = 300)
}
\arguments{
\item{p}{See Usage.}
\item{d1}{Passed to \code{.vcomp_f_cdf}.}
\item{d2}{Passed to \code{.vcomp_f_cdf}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.}
}
\description{
A step of the vcomp_native implementation. Called by \code{morie_vcomp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
