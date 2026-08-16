% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_design}
\alias{morie_prphet_design}
\title{morie_prphet_design}
\usage{
morie_prphet_design(t, cps, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0))
}
\arguments{
\item{t}{See Usage.}
\item{cps}{See Usage.}
\item{seasonalities}{Defaults to \code{NULL}.}
\item{holidays}{Defaults to \code{NULL}.}
\item{holiday_window}{Defaults to \code{c(0, 0)}.}
}
\description{
Part of the prphet_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{X}, \code{cols}, \code{holiday.names}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prphet_design(V, V)
}
