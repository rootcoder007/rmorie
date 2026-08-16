% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gwr_criterion}
\alias{.schab_gwr_criterion}
\title{.schab_gwr_criterion}
\usage{
.schab_gwr_criterion(y, X, distances, bandwidth, kernel = "gaussian", adaptive = FALSE, criterion = "cv")
}
\arguments{
\item{y}{Passed to \code{.schab_cv_score}.}
\item{X}{Passed to \code{.schab_cv_score}.}
\item{distances}{Passed to \code{.schab_cv_score}.}
\item{bandwidth}{Passed to \code{.schab_cv_score}.}
\item{kernel}{Passed to \code{.schab_cv_score}. Defaults to \code{"gaussian"}.}
\item{adaptive}{Passed to \code{.schab_cv_score}. Defaults to \code{FALSE}.}
\item{criterion}{One of \code{"aic"}, \code{"aicc"}, \code{"cv"}. Defaults to \code{"cv"}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_select_bandwidth}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.schab_gwr_criterion(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), distances = c(1, 2, 3, 4, 5, 6, 7, 8), bandwidth = 0.5)
}
