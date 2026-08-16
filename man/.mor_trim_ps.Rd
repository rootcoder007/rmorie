% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_trim_ps}
\alias{.mor_trim_ps}
\title{.mor_trim_ps}
\usage{
.mor_trim_ps(ps, trim = c(0.01, 0.99), trim_type = "value")
}
\arguments{
\item{ps}{See Usage.}
\item{trim}{Optional; may be \code{NULL}. A vector; indexed elementwise. Defaults to \code{c(0.01, 0.99)}.}
\item{trim_type}{One of \code{"discard"}, \code{"quantile"}, \code{"value"}. Defaults to \code{"value"}.}
}
\description{
A step of the causal implementation. Called by \code{morie_estimate_aipw}, \code{morie_estimate_ate}, \code{morie_estimate_propensity_scores}. See the file header for the source the module follows.
}
\value{
The value of \code{pmin}.
}
\keyword{internal}
