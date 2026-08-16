% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gwr_fit}
\alias{.schab_gwr_fit}
\title{.schab_gwr_fit}
\usage{
.schab_gwr_fit(y, X, distances, bandwidth, kernel = "gaussian", adaptive = FALSE)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{distances}{See Usage.}
\item{bandwidth}{See Usage.}
\item{kernel}{Defaults to \code{"gaussian"}.}
\item{adaptive}{Defaults to \code{FALSE}.}
}
\description{
Part of the schab_gwr_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{se_params}, \code{sigma2_gwr}, \code{edf_resid}, \code{v1}, \code{v2}, \code{params}, \code{fitted}, \code{resid}, \code{S}, \code{tr_S}, \code{tr_STS}, \code{effective_parameters}, \code{rss}, \code{sigma2}, \code{sigma2_cressie}, \code{n}, \code{p}, \code{bandwidth}, \code{kernel}, \code{adaptive}, \code{n_rank_deficient}.
}
\keyword{internal}
