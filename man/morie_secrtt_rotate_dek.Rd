% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_rotate_dek}
\alias{morie_secrtt_rotate_dek}
\title{Rotate a DEK: this one DOES re-encrypt the record}
\usage{
morie_secrtt_rotate_dek(sealed, old_dek, new_dek, new_nonce) 
}
\arguments{
\item{sealed}{List with \code{ciphertext}, \code{nonce}, \code{tag},   optional \code{aad}.}
\item{old_dek}{Raw DEK.}
\item{new_dek}{Raw DEK.}
\item{new_nonce}{Raw 12-byte nonce.}
}
\description{
Rotate a DEK: this one DOES re-encrypt the record
}
\value{
List with \code{sealed}, \code{records_reencrypted},   \code{note}.
}
\keyword{internal}
\examples{
if (morie_crypto_sodium_available()) {
  seed <- as.raw(1:32)
  dek1 <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
  dek2 <- morie_secrtt_generate_dek(seed, record_id = 8L)$dek
  s <- morie_secrtt_seal_record(charToRaw("hello"), dek1,
                                nonce = as.raw(13:24))
  r <- morie_secrtt_rotate_dek(s, dek1, dek2, new_nonce = as.raw(25:36))
  rawToChar(morie_secrtt_open_record(r$sealed, dek2))
}
}
