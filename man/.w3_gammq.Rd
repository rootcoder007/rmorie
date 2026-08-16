% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_gammq}
\alias{.w3_gammq}
\title{Regularised upper incomplete gamma Q(a, x). Computed without forming}
\usage{
.w3_gammq(a, x)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{x}{Passed to \code{.w3_gammp}.}
}
\description{
1 - P when x is large, so the far tail keeps its significant digits instead of cancelling against 1.
}
\value{
The value of \code{.w3_gammcf}.
}
\keyword{internal}
