% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Acfseg}
\alias{Acfseg}
\title{Section 8.5.2, eqs. (8.27)-(8.29), after Michael and Houchin}
\usage{
Acfseg(test, reference, lags = NULL, thp = 1, thf = 1)
}
\arguments{
\item{test}{See Usage.}
\item{reference}{See Usage.}
\item{lags}{Defaults to \code{NULL}.}
\item{thp}{Defaults to \code{1}.}
\item{thf}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_adapt implementation; see the file header for the source it follows.
}
\value{
A list with \code{distance}, \code{power_distance}, \code{spectral_distance}, \code{lags}, \code{lags_auto}, \code{acf_test}, \code{acf_reference}, \code{power_test}, \code{power_reference}, \code{boundary}, \code{th_power}, \code{th_spectral}, \code{amplitude_invariant}, \code{method}.
}
\keyword{internal}
