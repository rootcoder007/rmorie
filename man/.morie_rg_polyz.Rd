% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_polyz}
\alias{.morie_rg_polyz}
\title{.morie_rg_polyz}
\usage{
.morie_rg_polyz(coefs, z)
}
\arguments{
\item{coefs}{A vector; its length is taken.}
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
A step of the rangayyan_filt implementation. Called by \code{BwDigital}, \code{BwHp}, \code{Comb} and 6 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
