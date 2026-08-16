% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_predict}
\alias{morie_prphet_predict}
\title{morie_prphet_predict}
\usage{
morie_prphet_predict(fit, t.new, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0))
}
\arguments{
\item{fit}{See Usage.}
\item{t.new}{See Usage.}
\item{seasonalities}{Defaults to \code{NULL}.}
\item{holidays}{Defaults to \code{NULL}.}
\item{holiday_window}{Defaults to \code{c(0, 0)}.}
}
\description{
Part of the prphet_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
