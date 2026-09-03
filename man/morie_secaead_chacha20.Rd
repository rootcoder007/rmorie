% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secaead_chacha20}
\alias{morie_secaead_chacha20}
\title{XOR the data with the keystream from counter onward}
\usage{
morie_secaead_chacha20(key, counter, nonce, data) 
}
\arguments{
\item{key}{Passed to \code{morie_secaead_chacha20_block}.}
\item{counter}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nonce}{Passed to \code{morie_secaead_chacha20_block}.}
\item{data}{Passed to \code{.secaead_as_bytes}.}
}
\description{
A step of the secaead_native implementation. Called by \code{morie_secaead_aead_decrypt}, \code{morie_secaead_aead_encrypt}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
key <- as.raw(0:31)
nonce <- as.raw(c(0, 0, 0, 0, 0, 0, 0, 0x4a, 0, 0, 0, 0))
pt <- charToRaw("Ladies and Gentlemen of the class of \x2799: If I could offer you only one tip for the future,
  sunscreen would be it.")
ct <- morie_secaead_chacha20(key, 1, nonce, pt)
stopifnot(ct[1] == 0x6e, ct[2] == 0x2e, ct[3] == 0x35, ct[4] == 0x9a)
head(ct, 8)
}
