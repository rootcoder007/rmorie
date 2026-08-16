% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_fit}
\alias{morie_prphet_fit}
\title{morie_prphet_fit}
\usage{
morie_prphet_fit(t, y, n_changepoints = 10L, changepoint_range = 0.8, changepoints = NULL, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0), changepoint_prior = 0.05, ridge = 1e-08)
}
\arguments{
\item{t}{See Usage.}
\item{y}{See Usage.}
\item{n_changepoints}{Defaults to \code{10L}.}
\item{changepoint_range}{Defaults to \code{0.8}.}
\item{changepoints}{Defaults to \code{NULL}.}
\item{seasonalities}{Defaults to \code{NULL}.}
\item{holidays}{Defaults to \code{NULL}.}
\item{holiday_window}{Defaults to \code{c(0, 0)}.}
\item{changepoint_prior}{Defaults to \code{0.05}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
Part of the prphet_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{residual}, \code{coef}, \code{beta}, \code{columns}, \code{changepoints}, \code{deltas}, \code{k}, \code{m}, \code{trend}, \code{holiday.names}, \code{t}, \code{n}, \code{changepoint_prior}, \code{n.active.changepoints}, \code{sigma}, \code{seasonalities}, \code{method}.
}
\keyword{internal}
