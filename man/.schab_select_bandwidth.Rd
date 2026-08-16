% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_select_bandwidth}
\alias{.schab_select_bandwidth}
\title{.schab_select_bandwidth}
\usage{
.schab_select_bandwidth(y, X, coords, kernel = "gaussian", criterion = "cv", adaptive = FALSE, bounds = NULL, tol = 1e-04)
}
\arguments{
\item{y}{Passed to \code{.schab_gwr_criterion}.}
\item{X}{Passed to \code{.schab_gwr_criterion}.}
\item{coords}{Passed to \code{.schab_pairwise_distances}.}
\item{kernel}{Passed to \code{.schab_gwr_criterion}. Defaults to \code{"gaussian"}.}
\item{criterion}{Passed to \code{.schab_gwr_criterion}. Defaults to \code{"cv"}.}
\item{adaptive}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{bounds}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{tol}{Passed to \code{.schab_golden_section}. Defaults to \code{1e-04}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_mgwr_backfit}, \code{spgwrb}. See the file header for the source the module follows.
}
\value{
A list with \code{bandwidth}, \code{score}, \code{criterion}, \code{bounds}, \code{adaptive}.
}
\keyword{internal}
\examples{
.schab_select_bandwidth(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), coords = c(1, 2, 3, 4, 5, 6, 7, 8))
}
