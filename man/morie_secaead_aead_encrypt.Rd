% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secaead_aead_encrypt}
\alias{morie_secaead_aead_encrypt}
\alias{morie_secaead}
\title{Encrypt from counter 1, then authenticate AAD and ciphertext}
\usage{
morie_secaead_aead_encrypt(key, nonce, plaintext, aad = NULL) 

morie_secaead(key, nonce, plaintext, aad = NULL) 
}
\arguments{
\item{key}{Passed to \code{morie_secaead_poly1305_key_gen}.}
\item{nonce}{Passed to \code{morie_secaead_poly1305_key_gen}.}
\item{plaintext}{Passed to \code{morie_secaead_chacha20}.}
\item{aad}{Passed to \code{.secaead_mac_data}.}
}
\description{
A step of the secaead_native implementation. Called by \code{morie_secrtt_seal_record}, \code{morie_secrtt_wrap_dek}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{ciphertext}, \code{ciphertext_hex}, \code{tag}, \code{tag_hex}, \code{onetime_key}, \code{aad_len}, \code{ct_len}, \code{method}, \code{note}.
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
pt <- charToRaw("Ladies and Gentlemen of the class of \x2799: If I could offer you only one tip for the future, sunscreen would be it.")
r <- morie_secaead_aead_encrypt(key, nonce, pt, aad)
stopifnot(substr(r$tag_hex, 1, 8) == "1ae10b59")
r$tag_hex
}
