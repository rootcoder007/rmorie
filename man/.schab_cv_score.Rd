% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_cv_score}
\alias{.schab_cv_score}
\title{Spgwr::gwr.cv.f -- leave-one-out; y_i never predicts itself}
\usage{
.schab_cv_score(y, X, distances, bandwidth, kernel = "gaussian", adaptive = FALSE)
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
The value of \code{total}, as built in the body.
}
\keyword{internal}
