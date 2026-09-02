% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_secarg_compress}
\alias{morie_secarg_compress}
\title{Argon2: a memory-hard password hash built on BLAKE2b}
\usage{
morie_secarg_compress(X, Y) 
}
\description{
Argon2: a memory-hard password hash built on BLAKE2b. Sources: Biryukov, A., Dinu, D., Khovratovich, D. & Josefsson, S. Generation of Memory-Hard Functions for Password Hashing and Other Applications", 2016 IEEE EuroS&P, 292-302, Native implementation mirroring morie.fn.secarg exactly: the same prehash H_0 over every parameter, the same block init from H_0, the same J_1/J_2 reference indices and the same G (rows then columns) under BLAKE2b.
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{Y}{Argument \code{Y}; see Usage.}
}
\value{
A list with components \code{SIMPLIFY}.
}
\references{
(2021) "Argon2 Memory-Hard Function for Password Hashing and
Proof-of-Work Applications", RFC 9106, doi:10.17487/RFC9106.
Biryukov, A., Dinu, D. & Khovratovich, D. (2016) "Argon2: New
doi:10.1109/EuroSP.2016.31.
Saarinen, M.-J. & Aumasson, J.-P. (2015) "BLAKE2", RFC 7693,
doi:10.17487/RFC7693.
}
\keyword{internal}
