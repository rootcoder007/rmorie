% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.as_nugget}
\alias{.abcgp.as_nugget}
\title{.abcgp.as_nugget}
\usage{
.abcgp.as_nugget(nugget, n)
}
\arguments{
\item{nugget}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}, \code{.abcgp.mle_lengthscale}. See the file header for the source the module follows.
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
