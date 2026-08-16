% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_criterion}
\alias{.gwr_criterion}
\title{.gwr_criterion}
\usage{
.gwr_criterion(y, X, D, bw, kernel, adaptive, criterion)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{X}{Passed to \code{.gwr_cv_score}.}
\item{D}{Passed to \code{.gwr_cv_score}.}
\item{bw}{Passed to \code{.gwr_cv_score}.}
\item{kernel}{Passed to \code{.gwr_cv_score}.}
\item{adaptive}{Passed to \code{.gwr_cv_score}.}
\item{criterion}{One of \code{"aic"}, \code{"cv"}.}
}
\description{
A step of the gwrcal_native implementation. Called by \code{morie_gwrcal}. See the file header for the source the module follows.
}
\value{
The value of \code{.gwr_aicc}.
}
\keyword{internal}
