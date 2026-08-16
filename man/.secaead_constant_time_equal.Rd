% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.secaead_constant_time_equal}
\alias{.secaead_constant_time_equal}
\title{.secaead_constant_time_equal}
\usage{
.secaead_constant_time_equal(a, b)
}
\arguments{
\item{a}{Passed to \code{.secaead_as_bytes}.}
\item{b}{Passed to \code{.secaead_as_bytes}.}
}
\description{
A step of the secaead_native implementation. Called by \code{morie_secaead_aead_decrypt}. See the file header for the source the module follows.
}
\value{
A logical value.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.secaead_constant_time_equal(V, V)
}
