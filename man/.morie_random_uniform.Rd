% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_random_uniform}
\alias{.morie_random_uniform}
\title{N uniforms in the OPEN interval (0, 1). The open interval matters: a}
\usage{
.morie_random_uniform(n, seed = 0, stream = 0)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{stream}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
normal quantile at 0 or 1 is infinite, and (w + 0.5)/2^32 reaches neither endpoint.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_random_uniform(n = 5L)
}
