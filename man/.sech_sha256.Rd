% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sech_sha256}
\alias{.sech_sha256}
\title{Pure base-R SHA-256 (FIPS 180-4). Slow but exact, no package}
\usage{
.sech_sha256(bytes)
}
\arguments{
\item{bytes}{See Usage.}
}
\description{
A step of the sechsh_native implementation. Called by \code{.kdf_hmac}, \code{.sech_hmac}, \code{morie_sechsh_chain_entry} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
