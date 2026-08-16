% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secaead_poly1305_key_gen}
\alias{morie_secaead_poly1305_key_gen}
\title{Block 0 gives the one-time key; the message starts at 1}
\usage{
morie_secaead_poly1305_key_gen(key, nonce)
}
\arguments{
\item{key}{Passed to \code{morie_secaead_chacha20_block}.}
\item{nonce}{Passed to \code{morie_secaead_chacha20_block}.}
}
\description{
A step of the secaead_native implementation. Called by \code{morie_secaead_aead_decrypt}, \code{morie_secaead_aead_encrypt}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\references{
----------
Nir, Y. & Langley, A. (2018) "ChaCha20 and Poly1305 for IETF
Protocols", RFC 8439, doi:10.17487/RFC8439. Secs. 2.1-2.3, 2.5,
2.6, 2.8, 2.8.2.

Bernstein, D. J. (2008) "ChaCha, a variant of Salsa20", Workshop
Record of SASC 2008.

Bernstein, D. J. (2005) "The Poly1305-AES message-authentication
code", FSE 2005, LNCS 3557, 32-49, doi:10.1007/11502760_3.
}
\keyword{internal}
\examples{
key <- as.raw(c(0x80:0x8f, 0x90:0x9f))
nonce <- as.raw(c(0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7))
otk <- morie_secaead_poly1305_key_gen(key, nonce)
stopifnot(otk[1] == 0x8a, otk[2] == 0xd5)
head(otk, 8)
}
