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
