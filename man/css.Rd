% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{css}
\alias{css}
\title{css}
\usage{
css(w, ar = numeric(0), ma = numeric(0), full = FALSE)
}
\arguments{
\item{w}{See Usage.}
\item{ar}{Defaults to \code{numeric(0)}.}
\item{ma}{Defaults to \code{numeric(0)}.}
\item{full}{Defaults to \code{FALSE}.}
}
\description{
Part of the sarima_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{ssq}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
css(V)
}
