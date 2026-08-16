% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sechsh_verify_chain}
\alias{morie_sechsh_verify_chain}
\title{Verify a hash-chained audit log}
\usage{
morie_sechsh_verify_chain(entries, hashes, key = NULL, genesis = .SECH_GEN)
}
\arguments{
\item{entries}{List of entries.}
\item{hashes}{List of expected hashes.}
\item{key}{Optional raw key.}
\item{genesis}{Raw 32-byte genesis.}
}
\description{
Returns the FIRST bad index; everything before it is still evidence and everything after it is not.
}
\value{
List with \code{intact} (==\code{estimate}),   \code{first_bad}, \code{verified_through}, \code{n}, \code{method},   \code{note}.
}
\keyword{internal}
