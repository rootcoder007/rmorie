% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secarg_variable_hash}
\alias{morie_secarg_variable_hash}
\title{Argon2\'s variable-length hash H\'}
\usage{
morie_secarg_variable_hash(data, length)
}
\arguments{
\item{data}{Raw vector of bytes.}
\item{length}{Positive integer target length.}
}
\description{
BLAKE2b stretched past 64 bytes; the first 32 bytes of every subsequent 64-byte block are kept so the output does not repeat.
}
\value{
Raw vector of \code{length} bytes.
}
\keyword{internal}
