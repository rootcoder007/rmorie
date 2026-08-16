% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secaead_poly1305_mac}
\alias{morie_secaead_poly1305_mac}
\title{The one-time authenticator over 2^130 - 5. key is 32 bytes: the}
\usage{
morie_secaead_poly1305_mac(message, key)
}
\arguments{
\item{message}{Passed to \code{.secaead_as_bytes}.}
\item{key}{Passed to \code{.secaead_as_bytes}.}
}
\description{
low 16 become r (clamped) and the high 16 become s.
}
\value{
The value of \code{.secaead_bytes_from_limbs}.
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
key <- as.raw(c(0x85, 0xd6, 0xbe, 0x78, 0x57, 0x55, 0x6d, 0x33,
                0x7f, 0x44, 0x52, 0xfe, 0x42, 0xd5, 0x06, 0xa8,
                0x01, 0x03, 0x80, 0x8a, 0xfb, 0x0d, 0xb2, 0xfd,
                0x4a, 0xbf, 0xf6, 0xaf, 0x41, 0x49, 0xf5, 0x1b))
msg <- charToRaw("Cryptographic Forum Research Group")
tag <- morie_secaead_poly1305_mac(msg, key)
stopifnot(tag[1] == 0xa8, tag[2] == 0x06, tag[3] == 0x1d, tag[4] == 0xc1)
tag
}
