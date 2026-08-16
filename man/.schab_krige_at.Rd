% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_krige_at}
\alias{.schab_krige_at}
\title{.schab_krige_at}
\usage{
.schab_krige_at(coords, z, target, sill, rng, mu)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{z}{Numeric; combined arithmetically in the body.}
\item{target}{See Usage.}
\item{sill}{Numeric; combined arithmetically in the body.}
\item{rng}{Numeric; combined arithmetically in the body.}
\item{mu}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
