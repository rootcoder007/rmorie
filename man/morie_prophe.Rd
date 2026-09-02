% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prophe}
\alias{morie_prophe}
\title{morie_prophe}
\usage{
morie_prophe(t, y, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 
    0), ...) 
}
\arguments{
\item{t}{Passed to \code{prophe_additive_components}.}
\item{y}{Passed to \code{prophe_additive_components}.}
\item{seasonalities}{Passed to \code{prophe_additive_components}.}
\item{holidays}{Passed to \code{prophe_additive_components}.}
\item{holiday_window}{Passed to \code{prophe_additive_components}. Defaults to \code{c(0, 0)}.}
\item{...}{Passed through.}
}
\description{
A step of the prophe_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{prophe_additive_components}.
}
\keyword{internal}
\examples{
set.seed(1)
t <- 1:60
y <- sin(2 * pi * t / 12) + 0.05 * t + rnorm(60) * 0.1
morie_prophe(t, y)
}
