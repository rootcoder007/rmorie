% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_profiled_reml}
\alias{.schab_profiled_reml}
\title{.schab_profiled_reml}
\usage{
.schab_profiled_reml(coords, z, X, nugget_ratio, rng, model)
}
\arguments{
\item{coords}{Passed to \code{.schab_correlation_matrix}.}
\item{z}{A matrix; passed to \code{crossprod}.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{nugget_ratio}{Passed to \code{.schab_correlation_matrix}.}
\item{rng}{Passed to \code{.schab_correlation_matrix}.}
\item{model}{Passed to \code{.schab_correlation_matrix}.}
}
\description{
A step of the schab_reml_shared implementation. Called by \code{.schab_fit_reml}. See the file header for the source the module follows.
}
\value{
A list with \code{value}, \code{gradient}, \code{sigma2}, \code{beta}.
}
\keyword{internal}
