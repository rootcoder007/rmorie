% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_crypto_shred}
\alias{morie_secrtt_crypto_shred}
\title{Report the scope of destroying a KEK}
\usage{
morie_secrtt_crypto_shred(kek_id, wrapped_deks)
}
\arguments{
\item{kek_id}{Identifier of the KEK to be destroyed.}
\item{wrapped_deks}{List of wrapped DEKs.}
}
\description{
A partial rotation leaves records readable under the other KEK, so the scope has to be stated for "we destroyed the key" to be a deletion claim.
}
\value{
List with \code{kek_id}, \code{records_shredded},   \code{indices}, \code{still_recoverable}, \code{complete},   \code{note}.
}
\keyword{internal}
