% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secaead_aead_decrypt}
\alias{morie_secaead_aead_decrypt}
\title{morie_secaead_aead_decrypt}
\usage{
morie_secaead_aead_decrypt(key, nonce, ciphertext, tag, aad = NULL)
}
\arguments{
\item{key}{Passed to \code{morie_secaead_poly1305_key_gen}.}
\item{nonce}{Passed to \code{morie_secaead_poly1305_key_gen}.}
\item{ciphertext}{Passed to \code{.secaead_mac_data}.}
\item{tag}{Passed to \code{.secaead_constant_time_equal}.}
\item{aad}{Passed to \code{.secaead_mac_data}.}
}
\description{
A step of the secaead_native implementation. Called by \code{morie_secrtt_open_record}, \code{morie_secrtt_unwrap_dek}. See the file header for the source the module follows.
}
\value{
A list with \code{valid}, \code{plaintext}, \code{expected_tag}.
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
nonce <- as.raw(c(0x07, 0, 0, 0, 0x40, 0x41, 0x42, 0x43,
                  0x44, 0x45, 0x46, 0x47))
aad <- as.raw(c(0x50, 0x51, 0x52, 0x53, 0xc0, 0xc1, 0xc2, 0xc3,
                0xc4, 0xc5, 0xc6, 0xc7))
pt <- charToRaw("attack at dawn")
enc <- morie_secaead_aead_encrypt(key, nonce, pt, aad)
dec <- morie_secaead_aead_decrypt(key, nonce, enc$ciphertext, enc$tag, aad)
stopifnot(dec$valid, identical(as.raw(dec$plaintext), pt))
rawToChar(as.raw(dec$plaintext))
}
