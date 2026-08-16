% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_wp1d}
\alias{.ot_wp1d}
\title{.ot_wp1d}
\usage{
.ot_wp1d(x, y, p = 2)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{Otmsw}, \code{Otsw}, \code{Otws2}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ot_wp1d(V, V)
}
