% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03hcU}
\alias{.s03hcU}
\title{Matern eq. (3.4.7) p. 38 at n = 2: area of the union of two discs}
\usage{
.s03hcU(r, v)
}
\arguments{
\item{r}{Numeric; combined arithmetically in the body.}
\item{v}{Passed to \code{.s03lens}.}
}
\description{
A step of the hcoreg implementation. Called by \code{.s03hcK1}, \code{.s03hcK2}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.s03hcU(r = 5L, v = 5L)
}
