% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wpt}
\alias{Wpt}
\title{Wavelet packet decomposition (full binary tree, natural order)}
\usage{
Wpt(x, wavelet = "db4", levels = 3)
}
\arguments{
\item{x}{See Usage.}
\item{wavelet}{Defaults to \code{"db4"}.}
\item{levels}{Defaults to \code{3}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{nodes}, \code{leaves}, \code{n_leaves}, \code{energy_per_leaf}, \code{dominant_leaf}, \code{entropy}, \code{levels}, \code{wavelet}, \code{method}.
}
\keyword{internal}
