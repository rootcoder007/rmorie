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
\item{key}{See Usage.}
\item{nonce}{See Usage.}
\item{ciphertext}{See Usage.}
\item{tag}{See Usage.}
\item{aad}{Defaults to \code{NULL}.}
}
\description{
Part of the secaead_native implementation; see the file header for the source it follows.
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
