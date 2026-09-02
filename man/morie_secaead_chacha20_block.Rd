% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secaead_chacha20_block}
\alias{morie_secaead_chacha20_block}
\title{One 64-byte keystream block. The permuted state is ADDED to the}
\usage{
morie_secaead_chacha20_block(key, counter, nonce, rounds = 20) 
}
\arguments{
\item{key}{Passed to \code{.secaead_as_bytes}.}
\item{counter}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nonce}{Passed to \code{.secaead_as_bytes}.}
\item{rounds}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
}
\description{
original, which is what stops the block function being invertible.
}
\value{
The value of \code{.secaead_le_bytes}.
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
nonce <- as.raw(c(0, 0, 0, 9, 0, 0, 0, 0x4a, 0, 0, 0, 0))
ks <- morie_secaead_chacha20_block(key, 1, nonce)
stopifnot(ks[1] == 0x10, ks[2] == 0xf1, ks[3] == 0xe7)
head(ks, 8)
}
