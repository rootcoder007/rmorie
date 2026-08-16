% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_empirical_variogram}
\alias{.schab_empirical_variogram}
\title{.schab_empirical_variogram}
\usage{
.schab_empirical_variogram(coords, z, n_lags = 10L)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{z}{A vector; indexed elementwise.}
\item{n_lags}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{gamma}, \code{counts}.
}
\keyword{internal}
