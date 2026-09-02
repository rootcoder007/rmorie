% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_open_record}
\alias{morie_secrtt_open_record}
\title{Decrypt a record; fails closed}
\usage{
morie_secrtt_open_record(sealed, dek) 
}
\arguments{
\item{sealed}{List with \code{ciphertext}, \code{nonce}, \code{tag},   optional \code{aad}.}
\item{dek}{Raw DEK.}
}
\description{
Decrypt a record; fails closed
}
\value{
Raw plaintext.
}
\keyword{internal}
\examples{
seed <- as.raw(1:32)
dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
s <- morie_secrtt_seal_record(charToRaw("hello"), dek,
                              nonce = as.raw(13:24))
rawToChar(morie_secrtt_open_record(s, dek))
}
