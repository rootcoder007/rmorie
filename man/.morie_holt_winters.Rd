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
\item{y}{A vector; its length is taken and its elements indexed.}
\item{period}{Coerced to integer by the body, with \code{as.integer}.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{beta}{Numeric; combined arithmetically in the body.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{horizon}{Optional; may be \code{NULL}. A count; the body uses it as \code{seq_len(...)}.}
\item{mult}{A flag; the body branches on it.}
}
\description{
A step of the forecast_native implementation. Called by \code{morie_holt_winters_additive}, \code{morie_holt_winters_mult}. See the file header for the source the module follows.
}
\value{
A list with \code{forecast}, \code{level}, \code{trend}, \code{seasonal}, \code{fitted}, \code{residuals}, \code{sse}, \code{alpha}, \code{beta}, \code{gamma}, \code{period}, \code{horizon}.
}
\keyword{internal}
