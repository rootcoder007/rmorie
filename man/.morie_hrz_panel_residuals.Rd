% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hrz_panel_residuals}
\alias{.morie_hrz_panel_residuals}
\title{.morie_hrz_panel_residuals}
\usage{
.morie_hrz_panel_residuals(y, x, beta)
}
\arguments{
\item{y}{A matrix; the body checks with \code{is.matrix}.}
\item{x}{A matrix; indexed by row and column.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the horowitz_native7 implementation. Called by \code{morie_panel_deconvolution}, \code{morie_panel_densities}, \code{morie_smoothed_fU}. See the file header for the source the module follows.
}
\value{
A list with \code{W}, \code{eta}, \code{n}, \code{T}.
}
\keyword{internal}
\examples{
.morie_hrz_panel_residuals(y = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), x = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), beta = 0.5)
}
