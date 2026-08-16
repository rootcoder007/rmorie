% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prophe}
\alias{morie_prophe}
\title{morie_prophe}
\usage{
morie_prophe(t, y, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0), ...)
}
\arguments{
\item{t}{See Usage.}
\item{y}{See Usage.}
\item{seasonalities}{Defaults to \code{NULL}.}
\item{holidays}{Defaults to \code{NULL}.}
\item{holiday_window}{Defaults to \code{c(0, 0)}.}
\item{...}{Passed through.}
}
\description{
A step of the prophe_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{prophe_additive_components}.
}
\keyword{internal}
