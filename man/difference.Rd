% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{difference}
\alias{difference}
\title{difference}
\usage{
difference(y, d = 0, D = 0, s = 1)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{d}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{0}.}
\item{D}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{0}.}
\item{s}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.sarima_fit}, \code{.sarimax_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{w}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
difference(V)
}
