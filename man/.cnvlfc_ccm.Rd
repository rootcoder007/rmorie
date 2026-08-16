% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cnvlfc_ccm}
\alias{.cnvlfc_ccm}
\title{.cnvlfc_ccm}
\usage{
.cnvlfc_ccm(x, y, E = 2L, tau = 1L, lib_sizes = NULL, seed = 1L, exclude = 0L)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{E}{Passed to \code{.cnvlfc_embed}. Defaults to \code{2L}.}
\item{tau}{Passed to \code{.cnvlfc_embed}. Defaults to \code{1L}.}
\item{lib_sizes}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Passed to \code{.cnvlfc_cross_map}. Defaults to \code{1L}.}
\item{exclude}{Passed to \code{.cnvlfc_cross_map}. Defaults to \code{0L}.}
}
\description{
A step of the cnvlfc_native implementation. Called by \code{cnvlfc_ccm}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{x_causes_y}, \code{y_causes_x}, \code{lib_sizes}, \code{E}, \code{tau}, \code{n_embeddable}, \code{verdict}, \code{method}.
}
\keyword{internal}
