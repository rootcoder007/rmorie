% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sample_bound}
\alias{sample_bound}
\title{sample_bound}
\usage{
sample_bound(n, r, C = 1, exponent = 1.2)
}
\arguments{
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{r}{Coerced to integer by the body, with \code{as.integer}.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{exponent}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.2}.}
}
\description{
A step of the meglt_native implementation. Called by \code{morie_meglt}. See the file header for the source the module follows.
}
\value{
A list with \code{m}, \code{fraction}, \code{n}, \code{r}, \code{exponent}, \code{note}.
}
\keyword{internal}
\examples{
sample_bound(n = 5L, r = 5L)
}
