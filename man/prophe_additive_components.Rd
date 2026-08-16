% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{prophe_additive_components}
\alias{prophe_additive_components}
\title{prophe_additive_components}
\usage{
prophe_additive_components(t, y, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0), ...)
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
Part of the prophe_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{components}, \code{total}, \code{fitted}, \code{residual}, \code{reconstruction_error}, \code{reconstructs}, \code{coef}, \code{changepoints}, \code{sigma}, \code{n}, \code{component_names}, \code{method}.
}
\keyword{internal}
