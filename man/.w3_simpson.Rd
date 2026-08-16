% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_simpson}
\alias{.w3_simpson}
\title{Composite Simpson on n panels (rounded up to an even number). Fixed}
\usage{
.w3_simpson(f, a, b, n = 200L)
}
\arguments{
\item{f}{Accepted by the signature and not used anywhere in the body.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
\item{n}{Numeric; combined arithmetically in the body. Defaults to \code{200L}.}
}
\description{
panels, not adaptive: an adaptive rule branches on a tolerance test and the two arms would take different numbers of subdivisions on the very integrals where it matters.
}
\value{
A numeric value.
}
\keyword{internal}
