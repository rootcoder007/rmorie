% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_garch_pack}
\alias{.morie_garch_pack}
\title{.morie_garch_pack}
\usage{
.morie_garch_pack(spec, x)
}
\arguments{
\item{spec}{One of \code{"aparch"}, \code{"gjr"}, \code{"igarch"}.}
\item{x}{A vector; indexed elementwise.}
}
\description{
A step of the garch_coint_native implementation. Called by \code{morie_garch_spec_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{p}, as built in the body.
}
\keyword{internal}
\examples{
.morie_garch_pack(spec = 5L, x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
