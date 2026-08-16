% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bkw_sdel}
\alias{.bkw_sdel}
\title{S_(i): the residual sd with observation i removed}
\usage{
.bkw_sdel(sse, e_i, h_i, n, p)
}
\arguments{
\item{sse}{Numeric; combined arithmetically in the body.}
\item{e_i}{Numeric; combined arithmetically in the body.}
\item{h_i}{Numeric; combined arithmetically in the body.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dffit implementation. Called by \code{Covrat}, \code{Dfbetb}, \code{Dffit}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.bkw_sdel(sse = c(1, 2, 3, 4, 5, 6, 7, 8), e_i = c(1, 2, 3, 4, 5, 6, 7, 8), h_i = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, p = 0.5)
}
