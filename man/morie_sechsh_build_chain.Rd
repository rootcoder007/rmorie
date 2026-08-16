% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sechsh_build_chain}
\alias{morie_sechsh_build_chain}
\title{Build a hash-chained audit log}
\usage{
morie_sechsh_build_chain(entries, key = NULL, genesis = .SECH_GEN)
}
\arguments{
\item{entries}{List of raw-byte entries.}
\item{key}{Optional raw key.}
\item{genesis}{Raw 32-byte genesis (default all zeros).}
}
\description{
Build a hash-chained audit log
}
\value{
List with \code{hashes}, \code{head}, \code{n},   \code{head_hex}, \code{keyed}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sechsh_build_chain(V)
}
