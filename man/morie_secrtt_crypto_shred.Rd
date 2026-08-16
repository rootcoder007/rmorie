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
\examples{
seed <- as.raw(1:32)
dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
kek <- as.raw(rev(1:32))
w1 <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(1:12), kek_id = "kek-1")
w2 <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(2:13), kek_id = "kek-2")
morie_secrtt_crypto_shred("kek-1", list(w1, w2))
}
