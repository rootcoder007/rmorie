% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kdf_hmac}
\alias{.kdf_hmac}
\title{Re-uses the same SHA-256 / HMAC primitives as sechsh via the local}
\usage{
.kdf_hmac(key, msg)
}
\arguments{
\item{key}{A vector; its length is taken.}
\item{msg}{See Usage.}
}
\description{
.sech_sha256 and .sech_hmac defined there; this file is intended to be sourced together with sechsh_native.R.
}
\value{
The value of \code{.sech_sha256}.
}
\keyword{internal}
