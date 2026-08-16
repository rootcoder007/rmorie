% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sechsh_chain_entry}
\alias{morie_sechsh_chain_entry}
\title{One step of a hash-chained audit log}
\usage{
morie_sechsh_chain_entry(previous_hash, entry, key = NULL)
}
\arguments{
\item{previous_hash}{Raw bytes.}
\item{entry}{Raw bytes.}
\item{key}{Optional raw key.}
}
\description{
h_i = H(h_{i-1} || e_i), or HMAC if a key is given. With a key the log writer does not hold, an attacker with write access still cannot recompute the chain forward.
}
\value{
List with \code{hash} and \code{keyed} (and \code{note}   when keyed).
}
\keyword{internal}
