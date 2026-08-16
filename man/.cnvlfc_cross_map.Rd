% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cnvlfc_cross_map}
\alias{.cnvlfc_cross_map}
\title{.cnvlfc_cross_map}
\usage{
.cnvlfc_cross_map(driver, response, E = 2L, tau = 1L, library = NULL, seed = 1L, exclude = 0L)
}
\arguments{
\item{driver}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{response}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{E}{Passed to \code{.cnvlfc_embed}. Defaults to \code{2L}.}
\item{tau}{Passed to \code{.cnvlfc_embed}. Defaults to \code{1L}.}
\item{library}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1L}.}
\item{exclude}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the cnvlfc_native implementation. Called by \code{.cnvlfc_ccm}, \code{cnvlfc_cross_map}. See the file header for the source the module follows.
}
\value{
A list with \code{rho}, \code{observed}, \code{predicted}, \code{n_predicted}, \code{library}, \code{E}, \code{tau}.
}
\keyword{internal}
