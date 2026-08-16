% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secarg_prehash}
\alias{morie_secarg_prehash}
\title{Argon2 prehash H_0 over every parameter}
\usage{
morie_secarg_prehash(password, salt, parallelism, tag_length, memory, passes, variant = "argon2id", secret = raw(), associated = raw(), version = .VERSION)
}
\arguments{
\item{password}{Raw bytes.}
\item{salt}{Raw bytes, at least 8.}
\item{parallelism}{Positive integer.}
\item{tag_length}{Positive integer.}
\item{memory}{Memory in KiB.}
\item{passes}{Positive integer.}
\item{variant}{One of \code{argon2d}, \code{argon2i}, \code{argon2id}.}
\item{secret}{Raw bytes (optional).}
\item{associated}{Raw bytes (optional).}
\item{version}{Integer (default 0x13).}
}
\description{
Changing any of the parameters changes the digest, so a tag cannot silently be compared across configurations.
}
\value{
Raw 64-byte BLAKE2b digest.
}
\keyword{internal}
