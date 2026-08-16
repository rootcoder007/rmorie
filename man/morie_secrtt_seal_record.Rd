% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_seal_record}
\alias{morie_secrtt_seal_record}
\title{Encrypt a record under its own DEK}
\usage{
morie_secrtt_seal_record(plaintext, dek, nonce, aad = raw())
}
\arguments{
\item{plaintext}{Raw bytes.}
\item{dek}{Raw 32-byte DEK.}
\item{nonce}{Raw 12-byte nonce.}
\item{aad}{Raw additional authenticated data.}
}
\description{
Encrypt a record under its own DEK
}
\value{
List with \code{ciphertext}, \code{tag}, \code{nonce},   \code{aad}.
}
\keyword{internal}
