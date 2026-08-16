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
