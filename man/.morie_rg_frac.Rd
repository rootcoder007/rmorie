% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_frac}
\alias{.morie_rg_frac}
\title{An exact rational as a list, so counts-based ratios are not rounded}
\usage{
.morie_rg_frac(n, d)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{d}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_class implementation. Called by \code{.morie_rg_asfrac}, \code{.morie_rg_fadd}, \code{.morie_rg_fmul} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{structure}.
}
\keyword{internal}
\examples{
.morie_rg_frac(n = 5L, d = 5L)
}
