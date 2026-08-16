% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_seckdf_expand}
\alias{morie_seckdf_expand}
\title{HKDF Expand}
\usage{
morie_seckdf_expand(prk, info = raw(), length = 32L)
}
\arguments{
\item{prk}{Raw pseudorandom key (at least HashLen bytes).}
\item{info}{Optional raw context info.}
\item{length}{Positive integer output length in bytes.}
}
\description{
Counter-mode expansion; the counter is a single octet so output is capped at \code{255 * HashLen}.
}
\value{
List with \code{okm}, \code{blocks}, \code{length}.
}
\keyword{internal}
