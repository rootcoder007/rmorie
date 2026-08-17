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
\examples{
prk <- morie_seckdf_extract(rep(as.raw(0x0b), 22), as.raw(0:12))$prk
info <- as.raw(c(0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
                 0xf8, 0xf9))
r <- morie_seckdf_expand(prk, info, 42L)
str(r, max.level = 1)
}
