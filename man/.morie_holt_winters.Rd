% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_holt_winters}
\alias{.morie_holt_winters}
\title{One Holt-Winters recursion, additive or multiplicative}
\usage{
.morie_holt_winters(y, period, alpha, beta, gamma, horizon, mult)
}
\arguments{
\item{y}{See Usage.}
\item{period}{See Usage.}
\item{alpha}{See Usage.}
\item{beta}{See Usage.}
\item{gamma}{See Usage.}
\item{horizon}{See Usage.}
\item{mult}{See Usage.}
}
\description{
Part of the forecast_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{forecast}, \code{level}, \code{trend}, \code{seasonal}, \code{fitted}, \code{residuals}, \code{sse}, \code{alpha}, \code{beta}, \code{gamma}, \code{period}, \code{horizon}.
}
\keyword{internal}
