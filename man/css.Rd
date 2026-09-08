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
\item{w}{A vector; its length is taken and its elements indexed.}
\item{ar}{A vector; its length is taken and its elements indexed. Defaults to \code{numeric(0)}.}
\item{ma}{A vector; its length is taken and its elements indexed. Defaults to \code{numeric(0)}.}
\item{full}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.residual_column}, \code{.sarima_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{ssq}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
css(V)
}
