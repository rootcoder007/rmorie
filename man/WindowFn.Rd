% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WindowFn}
\alias{WindowFn}
\title{Section 3.4.  Truncating a record IS multiplying it by a rectangle,}
\usage{
WindowFn(N, window_type = "hamming")
}
\arguments{
\item{N}{Coerced to integer by the body, with \code{as.integer}.}
\item{window_type}{Passed to \code{\%in\%}. Defaults to \code{"hamming"}.}
}
\description{
whose transform has sidelobes that leak energy from strong components into neighbouring bins; a tapered window trades a wider main lobe for lower sidelobes.  The rectangular window is included because it is the default -- doing nothing is choosing it -- and naming it makes that choice explicit.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
WindowFn(N = 5L)
}
