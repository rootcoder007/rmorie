% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.secaead_limbs_from_bytes}
\alias{.secaead_limbs_from_bytes}
\title{Bs: integer bytes LSB first -> base 2^26 limbs}
\usage{
.secaead_limbs_from_bytes(bs)
}
\arguments{
\item{bs}{See Usage.}
}
\description{
A step of the secaead_native implementation. Called by \code{morie_secaead_poly1305_mac}. See the file header for the source the module follows.
}
\value{
The value of \code{limbs}, as built in the body.
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
